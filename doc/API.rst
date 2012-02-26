MESSAGE PASSING API
===================

This defines the message passing API between the conversation server and the web client.


Overview
--------

Shows event names. Replies and most parameters are omitted.


SERVER                CLIENT

                      connect
                      disconnect
                      list_channels
                      list_users (channel)
                      list_conversations (channel)
                      join (channel)
                      send_channel (channel)
                      send_conversation (channel, conversation)
disconnect
receive_channel
receive_conversation


JSON format convention
----------------------

All messages are wrapped into a carrier object.
This object contains the name of the event, the event data, and possibly more information (like timestamps or authentication tokens).
Extra information is omitted in the below examples.

Full JSON structure
^^^^^^^^^^^^^^^^^^^

    {
        event: 'eventName',
        data: {...}
        ...
    }

Example for a list_users event:

    {
        event: 'list_users',
        data: { channel: 'haskell' }
        ...
    }

Short-cut syntax
^^^^^^^^^^^^^^^^

For this specification, we usually use short-cut syntax. Example for the above:

    list_users { channel: 'haskell' }


Response convention
-------------------

Some messages (e.g. connect, list_users) require a response or can fail.
Responses are postfixed the responding side with '.ok' or '.error'.
Unspecific errors have the event name 'error'.

Example request:

    list_users { channel: 'haskell' }

Example response:

    list_users.ok { users: ['ex_falso', 'rostayob', 'scvalex', 'nh2'] }

Example specific error:

    list_users.error { reason: 'not_authenticated' }

Unknown errors:

    'error' { reason: 'database failure' }


Event details
-------------

This lists example usages of each event.
Error responses are usually omitted.


connect
^^^^^^^

Client:

    connect { user: 'scvalex' }

Server:

    connect.ok {}


disconnect (client-issued)
^^^^^^^^^^^^^^^^^^^^^^^^^^

Client:

    disconnect {}

Server:

    disconnect.ok {}


list_channels
^^^^^^^^^^^^^

Client:

    list_channels {}

Server:

    list_channels.ok { channels: ['haskell', 'icdoc-design'] }


list_users
^^^^^^^^^^

Client:

    list_users { channel: 'haskell' }

Server:

    list_users.ok { channel: 'haskell', users: ['rostayob', 'ex_falso'] }


list_conversations
^^^^^^^^^^^^^^^^^^

Client:

    list_conversations { channel: 'haskell' }

Server:

    list_conversations.ok {
        channel: 'haskell',
        conversations: { 'why_haskell': CONVERSATION, 'i_dont_understand_the_singleton_pattern': CONVERSATION }
    }

CONVERSATION:

    { tag: 'why_haskell', topic: 'Why Haskell?', users: ['scvalex', 'rostayob'] }


join
^^^^

Client:

    join { channel: 'haskell' }

Server:

    join.ok { channel: 'haskell' }


send_channel
^^^^^^^^^^^^

Client:

    send_channel { channel: 'haskell', message: 'Can I just open a new conversation?' }

Server:

    send_channel.ok {}


send_conversation
^^^^^^^^^^^^^^^^^

Client:

    send_conversation {
        channel: 'haskell',
        conversation: 'why_haskell',
        message: 'Why would you recommend Haskell for my next project?'
    }

Server:

    send_conversation.ok {}


disconnect (server-issued)
^^^^^^^^^^^^^^^^^^^^^^^^^^

Server:

    disconnect { reason: 'kicked_spam' }


receive_channel
^^^^^^^^^^^^^^^

Server:

    receive_channel {
        channel: 'haskell',
        user: 'scvalex',
        message: 'Yes, you can just open a new conversation.'
    }


receive_conversation
^^^^^^^^^^^^^^^^^^^^

Server:

    receive_conversation {
        channel: 'haskell',
        conversation: 'why_haskell',
        user: 'rostayob',
        message: 'Because Haskell can save you a lot of time.'
    }
