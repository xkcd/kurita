import sortBy from 'lodash/sortBy'

type ServerCompetitorState = {
  competitor: string,
  score: number,
}

type ServerGameState = {
  extra: {
    commentary: Array<string>,
    end_time: Date,
  },
  game: [ServerCompetitorState, ServerCompetitorState],
}

export type ServerBracketState = {
  upcoming: Array<[string, string]>,
  current: ServerGameState | null,
  played: Array<ServerGameState>,
}

type ServerStartEvent = {
  event: 'start',
  bracket: ServerBracketState,
}

type ServerScoreEvent = {
  event: 'score',
  scores: [ServerCompetitorState, ServerCompetitorState],
}

export type ServerEvent = (
  ServerStartEvent | ServerScoreEvent
)

type ServerAction = {
  event: 'vote',
  for: string,
}

type ClientOptions = {
  debug: boolean,
  endpoint: string,
}

type ServerEventHandler = (event: ServerEvent) => void

class KuritaClient {
  private ws: WebSocket
  private options: ClientOptions
  private listeners: Set<ServerEventHandler>
  private reconnectTimeout: number

  constructor(options: ClientOptions) {
    this.options = options
    this.listeners = new Set()
  }

  connect() {
    this.ws = new WebSocket(this.options.endpoint)
    this.ws.addEventListener('message', this.handleMessage)
    this.ws.addEventListener('close', this.handledisconnect)
  }

  disconnect() {
    this.ws.close()
  }

  private handledisconnect = () => {
    if (this.options.debug) {
      console.log('disconnected, reconnecting...')
    }
    clearTimeout(this.reconnectTimeout)
    setTimeout(() => { this.connect() }, Math.random() * 2500 + 2500)
  }

  subscribe(handler: ServerEventHandler) {
    this.listeners.add(handler)
    return () => {
      this.listeners.delete(handler)
    }
  }

  send(data: ServerAction) {
    const dataText = JSON.stringify(data)
    this.ws.send(dataText)
    if (this.options.debug) {
      console.log('sent', data)
    }
  }

  private _reviver(key: string, value: any) {
    if (key.endsWith('_time')) {
      return new Date(value)
    }
    if (key === 'game' || key === 'scores') {
      return sortBy(value, state => state.competitor)
    }
    return value
  }

  private handleMessage = (event: MessageEvent) => {
    const dataText = event.data
    const data = JSON.parse(dataText, this._reviver) as ServerEvent

    if (this.options.debug) {
      console.log('received', data)
    }

    for (const listener of this.listeners) {
      listener(data)
    }
  }
}

export default KuritaClient
