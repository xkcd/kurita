import clone from 'lodash/clone'
import {createContext, useCallback, useEffect, useReducer} from 'react'

import KuritaClient, {ServerEvent, ServerBracketState} from './KuritaClient'

type KuritaState = {
  bracket?: ServerBracketState,
}

type KuritaAction = (
  {
    type: 'receive-event',
    payload: ServerEvent,
  } | {
    type: 'send-vote',
    payload: string,
  }
)

const initialState: KuritaState = {
  bracket: undefined,
}

function reducer(state: KuritaState, action: KuritaAction): KuritaState {
  switch (action.type) {
    case 'receive-event':
      switch (action.payload.event) {
        case 'start':
          return {
            ...state,
            bracket: action.payload.bracket,
          }

        case 'score':
          const newState = clone(state)
          if (newState.bracket) {
            newState.bracket.current.game = action.payload.scores
          }
          return newState
      }

    case 'send-vote':
      const newState = clone(state)
      const votedCompetitor = newState.bracket.current.game.find(
        ({competitor}) => competitor === action.payload
      )
      votedCompetitor.score++
      return newState
      
    default:
      return state
  }
}

export function useKurita(client: KuritaClient): [KuritaState, (action: KuritaAction) => void] {
  const [state, dispatch] = useReducer(reducer, initialState)

  const fullDispatch = useCallback(
    (action: KuritaAction) => {
      if (action.type === 'send-vote') {
        client.send({
          event: 'vote',
          for: action.payload,
        })
      }
      dispatch(action)
    },
    [client],
  )

  useEffect(() => {
    const unsubscribe = client.subscribe(event => {
      dispatch({type: 'receive-event', payload: event})
    })
    return unsubscribe
  }, [client])

  return [state, fullDispatch]
}
