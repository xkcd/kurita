import React, {createContext, CSSProperties, useContext} from 'react'
import {useInView} from 'react-intersection-observer'

// from https://github.com/twitter/twemoji/blob/7914d152d65a5b3d8749e1768e9b25437ec9fd64/2/twemoji.js#L548-L566
function toCodePoint(unicodeSurrogates: string, sep: string = '-') {
  var
    r = [],
    c = 0,
    p = 0,
    i = 0;
  while (i < unicodeSurrogates.length) {
    c = unicodeSurrogates.charCodeAt(i++);
    if (p) {
      r.push((0x10000 + ((p - 0xD800) << 10) + (c - 0xDC00)).toString(16));
      p = 0;
    } else if (0xD800 <= c && c <= 0xDBFF) {
      p = c;
    } else {
      r.push(c.toString(16));
    }
  }
  return r.join(sep);
}

export const EmojiPath = createContext('')

const Emoji: React.FC<{
  className?: string,
  emoji: string,
  size: number,
  style?: CSSProperties,
}> = ({className, emoji, size, style}) => {
  const [viewRef, inView] = useInView({
    rootMargin: '100px 0px 100px 0px',
    triggerOnce: true,
  })

  const basePath = useContext(EmojiPath)
  const codePoint = toCodePoint(emoji)

  const placeholder = (
    <div
      ref={viewRef}
      className={className}
      style={{display: 'inline-block', width: size, height: size, ...style}}
    />
  )

  const emojiImg = (
    <img
      ref={viewRef}
      className={className}
      src={`${basePath}${codePoint}.svg`}
      alt={emoji}
      width={size}
      height={size}
      style={style}
    />
  )

  return inView ? emojiImg : placeholder
}

export default Emoji
