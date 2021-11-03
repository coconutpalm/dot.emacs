package org.eclipse.gef.ui.actions;

import org.eclipse.ui.IPartListener;

public interface IPartService {

	void addPartListener(IPartListener iPartListener);

	void removePartListener(IPartListener partListener);

}
