import isEqual from 'lodash/isEqual'
import sample from 'lodash/sample'
import React, {useLayoutEffect, useRef, useState} from 'react'
import styled, {keyframes} from 'styled-components'

import Emoji from './Emoji'
import useTime from './useTime'

const heartFlyTime = 1000
const maxHearts = 50
const heartEmojis = ['❤', '💚', '💙', '💜']
const HeartCounter: React.FC<{
  count: number
}> = ({count}) => {
  const [lastCount, setLastCount] = useState(null)
  const [hearts, setHearts] = useState([])
  const now = useTime(500)
  const nowMs = now.getTime()

  const containerRef = useRef(null)
  const [size, setSize] = useState([])
  const [width, height] = size
  useLayoutEffect(
    () => {
      const box = containerRef.current.getBoundingClientRect()
      setSize([box.width, box.height])
    },
    [],
  )

  const newHearts = hearts.filter(([emoji, seed, delay, endMs]) => endMs + heartFlyTime > nowMs)
  if (count > lastCount && lastCount !== null) {
    const actualCount = count - lastCount
    const logCount = Math.log(count - lastCount) / Math.log(1.5)
    const newCount = Math.max(Math.min(actualCount, 1), logCount)
    for (let i = 0; i < newCount; i++) {
      if (newHearts.length >= maxHearts) {
        break
      }

      const delay = Math.random()
      newHearts.push([
        sample(heartEmojis),
        Math.random(),
        delay,
        nowMs + heartFlyTime + delay * heartFlyTime,
      ])
    }
  }
  if (count != lastCount) {
    setLastCount(count)
  }

  if (!isEqual(hearts, newHearts)) {
    setHearts(newHearts)
  }

  return (
    <Fill ref={containerRef} style={{marginBottom: 32, marginTop: -32}}>
      {newHearts.map(([emoji, seed, delay, endMs]) => {
        const style = {
          left: width * seed,
          top: height,
          opacity: 0,
          animationDelay: `${delay * heartFlyTime}ms`,
        }
        return (
          <AnimatedEmoji key={seed} emoji={emoji} size={24} style={style} />
        )
      })}
    </Fill>
  )
}

const Fill = styled.div`
  position: absolute;
  left: 0;
  top: 0;
  right: 0;
  bottom: 0;
`

const rise = keyframes`
  0% {
    transform: translate(0px, 0px);
    opacity: 0;
  }

  25% {
    transform: translate(-25px, -50px);
    opacity: 1;
  }

  50% {
    transform: translate(0px, -100px);
  }

  75% {
    transform: translate(25px, -150px);
  }

  100% {
    transform: translate(0px, -200px);
    opacity: 0;
  }
`

const AnimatedEmoji = styled(Emoji)`
  position: absolute;
  animation: ${rise} ${heartFlyTime}ms linear;
`

export default HeartCounter
