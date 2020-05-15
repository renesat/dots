#!/usr/bin/env python

from subprocess import check_output


def get_pass(account):
    """ Return pass login and passwword
    """
    data = check_output("/usr/bin/env pass mail/" + account,
                        shell=True).splitlines()
    password = data[0]
    tmp = [
        x for x in data
        if x.lower().startswith(('address:', 'login:', 'user:'))
    ]
    user = ""
    if tmp:
        user = tmp[0].split(":", 1)[1].strip()

    return {"password": password, "user": user}


def folder_filter(name):
    """ Filter folder
    """
    return (name in ["Archives", "Drafts", "INBOX", "Sent", "Spam", "Trash"]
            or name.startswith("Mailspring"))
