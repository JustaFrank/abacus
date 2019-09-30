import { useEffect, RefObject } from 'react'

export const useMousedown = (handleClick: (event: MouseEvent) => any) => {
  useEffect(() => {
    document.addEventListener('mousedown', handleClick)
    return () => document.removeEventListener('mousedown', handleClick)
  }, [handleClick])
}

export const useMousedownOutside = (
  ref: RefObject<HTMLElement>,
  handleOutsideClick: () => any
) => {
  const handleClick = (event: MouseEvent) => {
    if (ref.current && !ref.current.contains(event.target as Element)) {
      handleOutsideClick()
    }
  }
  useMousedown(handleClick)
}
