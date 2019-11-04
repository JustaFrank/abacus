import React, { useState } from 'react'
import { RouteComponentProps, navigate } from '@reach/router'
import styled from 'styled-components'

import { Button } from '../common/Button'
import { Page, PageDescription, PageHeading } from '../page/Page'
import { useForm } from '../../hooks/use-form'
import { useUser } from '../../context/user-context'

const Input = styled.input`
  border: 1px solid lightgray;
  border-radius: 4px;
  outline: none;
  font: inherit;
  padding: 4px 8px;
  height: inherit;

  :focus {
    border-color: #5a6c7c;
    box-shadow: 0 0 0 2px rgba(90, 108, 124, 0.25);
  }
`

const TextArea = styled.textarea`
  width: 100%;
  box-sizing: border-box;
  border: 1px solid lightgray;
  border-radius: 4px;
  outline: none;
  font: inherit;
  padding: 8px 8px;
  height: inherit;

  :focus {
    border-color: #5a6c7c;
    box-shadow: 0 0 0 2px rgba(90, 108, 124, 0.25);
  }
`

const Label = styled.div`
  font-size: 14px;
  font-weight: bolder;
  margin-bottom: 8px;
`

const InputContainer = styled.div`
  padding-bottom: 12px;
`

export const Create: React.FC<RouteComponentProps> = () => {
  const [isLoading, setIsLoading] = useState(false)

  const { publishFunction } = useUser()
  const { props, values } = useForm({
    name: '',
    description: '',
    body: 'function nameOfYourFunction(arg1, arg2) {\n  // Your code here\n}'
  })

  const handleSubmit = async () => {
    setIsLoading(true)
    await publishFunction(values.name, values.description, values.body)
    setIsLoading(false)
    navigate('/calculator')
  }

  return (
    <Page>
      <PageHeading>Create</PageHeading>
      <PageDescription>
        Create a custom Javascript function. Function must have the format:
        <span style={{ fontFamily: 'Consolas', fontSize: '12px' }}>
          {' '}
          function nameOfFunction() {'{}'}.
        </span>
      </PageDescription>
      <InputContainer>
        <Label>name</Label>
        <Input {...props.name} type="text" />
      </InputContainer>
      <InputContainer>
        <Label>description</Label>
        <TextArea {...props.description} rows={3} />
      </InputContainer>
      <InputContainer>
        <Label>code</Label>
        <TextArea
          {...props.body}
          style={{ fontFamily: 'Consolas' }}
          rows={15}
        />
      </InputContainer>
      <Button
        width="160px"
        backgroundColor="orange"
        color="black"
        style={{ marginTop: '12px' }}
        onClick={handleSubmit}
        loading={isLoading}
      >
        publish
      </Button>
    </Page>
  )
}
