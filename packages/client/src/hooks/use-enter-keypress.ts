export const useEnterKeypress = (handleEnter: () => any) => {
  const handleKeyPress = (event: React.KeyboardEvent) => {
    if (event.charCode === 13) {
      handleEnter()
    }
  }
  return handleKeyPress
}
