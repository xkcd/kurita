import {useEffect, useState} from 'react'

function useTime(intervalMs: number) {
  const [now, setNow] = useState(new Date())
  useEffect(() => {
    const timeout = setInterval(() => { setNow(new Date()) }, intervalMs)
    return () => { clearInterval(timeout) }
  }, [])

  return now
}

export default useTime
