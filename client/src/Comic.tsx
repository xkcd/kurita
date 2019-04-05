import flatten from 'lodash/flatten'
import ms from 'ms'
import React, {useEffect, useCallback, useMemo, useState} from 'react'
import styled from 'styled-components'

import KuritaClient from './KuritaClient'
import {useKurita} from './KuritaState'
import Emoji, {EmojiPath} from './Emoji'
import HeartCounter from './HeartCounter'
import Text from './Text'
import useTime from './useTime'

const unknownEmoji = '❓'

function timeUntil(now: Date, time: Date) {
  const nowMs = now.getTime()
  const timeMs = time.getTime()
  if (timeMs - nowMs < 3000) {
    return 'time\'s up!'
  }
  return ms(time.getTime() - now.getTime(), {long: true})
}

const Comic: React.FC<{
  altText: string,
  client: KuritaClient,
  emojiBasePath: string,
}> = ({altText, client, emojiBasePath}) => {
  const [state, dispatch] = useKurita(client)
  const now = useTime(1000)

  const voteCallback = useCallback(
    (emoji: string) => { dispatch({type: 'send-vote', payload: emoji}) },
    [dispatch],
  )

  const current = state.bracket && state.bracket.current
  const played = state.bracket && state.bracket.played

  const history = useMemo(() => (
    state.bracket && flatten(played).map(gameState => {
      const [emoji1, emoji2] = gameState.game
      const firstWins = emoji1.score > emoji2.score
      return (
        <div key={emoji1.competitor + emoji2.competitor} style={{display: 'flex', margin: '8px 0'}}>
          <HistoryEffect underline={firstWins} grayscale={!firstWins}>
            <Emoji emoji={emoji1.competitor} size={32} />
          </HistoryEffect>
          <Text size={16} style={{alignSelf: 'center', margin: '0 8px'}}>vs</Text>
          <HistoryEffect underline={!firstWins} grayscale={firstWins}>
            <Emoji emoji={emoji2.competitor} size={32} />
          </HistoryEffect>
          <Text parseEmoji size={16} style={{alignSelf: 'center', marginLeft: '14px'}}>
            "{gameState.extra.commentary[0]}"
          </Text>
        </div>
      )
    })
  ), [played])

  return (
    <EmojiPath.Provider value={emojiBasePath}>
      <ComicOutline title={altText}>
        <div style={{display: 'flex', flex: 1, flexDirection: 'column', alignItems: 'center', justifyContent: 'center'}}>
          <Text parseEmoji size={20} style={{margin: 24, height: '1em'}}>
            {current && `🤖 "${current.extra.commentary[0]}"`}
          </Text>
          <div style={{display: 'flex', justifyContent: 'center', opacity: current ? 1 : .5}}>
            <EmojiVoteBox disabled={!current} score={current && current.game[0].score} emoji={current ? current.game[0].competitor : unknownEmoji} onVote={voteCallback} />
            <Text size={32} style={{alignSelf: 'center', margin: 20}}>vs</Text>
            <EmojiVoteBox disabled={!current} score={current && current.game[1].score} emoji={current ? current.game[1].competitor : unknownEmoji} onVote={voteCallback} />
          </div>
          <div style={{marginTop: 20}}>
            <Text size={20}>Remaining time:&nbsp;</Text>
            <Text size={20} style={{display: 'inline-block', width: '5.25em', textAlign: 'right', opacity: .75, whiteSpace: 'nowrap'}}>{current && timeUntil(now, current.extra.end_time)}</Text>
          </div>
        </div>
        <div style={{margin: 16, marginTop: 0}}>
          <Text size={16}>Past bouts:</Text>
          <ScrollingHistoryBox>
            {history}
          </ScrollingHistoryBox>
        </div>
        <a href="/2131/emojidome_bracket_256.png" style={{alignSelf: 'center', marginBottom: 8}}><Text size={16}>Full bracket for today's comic (round 2)</Text></a>
      </ComicOutline>
    </EmojiPath.Provider>
  )
}

const EmojiVoteBox: React.FC<{
  disabled?: boolean,
  emoji: string,
  score?: number,
  onVote: (emoji: string) => void,
}> = ({disabled, emoji, onVote, score}) => {
  const onVoteCallback = useCallback(
    () => { onVote(emoji) },
    [onVote, emoji],
  )

  return (
    <div style={{position: 'relative', display: 'flex', flexDirection: 'column'}}>
      <HeartCounter count={score} />
      <Emoji emoji={emoji} size={128} />
      <VoteButton
        disabled={disabled}
        onClick={onVoteCallback}
        aria-label={`Vote for ${emoji}`}
      >
        Vote
      </VoteButton>
    </div>
  )
}

const ComicOutline = styled.div`
  display: inline-flex;
  flex-direction: column;
  box-sizing: border-box;
  border: 2px solid black;
  width: 740px;
  height: 500px;
  user-select: none;
  text-align: left;
`

const ScrollingHistoryBox = styled.div`
  height: 138px;
  overflow-y: auto;
`

interface HistoryEffectProps {
  grayscale?: boolean,
  underline?: boolean,
}
const HistoryEffect = styled.div<HistoryEffectProps>`
  display: flex;
  padding-bottom: 3px;
  ${({grayscale}: HistoryEffectProps) => grayscale ? 'filter: grayscale(100%)' : ''}
  ${({underline}: HistoryEffectProps) => underline ? 'border-bottom: 2px solid green' : ''}
`

const VoteButton = styled.button`
  position: relative;
  z-index: 9999;
  border: 2px solid black;
  border-radius: 4px;
  background: #ccc;
  font-family: xkcd-Regular-v2;
  font-size: 24px;
  line-height: 24px;
  padding: 4px 8px;
  margin-top: 20px;
  cursor: pointer;
  transition: background-color .25s ease-out;

  &:active {
    transition: none;
    background-color: red;
  }
`

export default Comic
