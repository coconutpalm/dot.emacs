package org.eclipse.gef.ui.properties;

public interface IPropertySource {

	boolean isPropertySet(Object propertyId);

	boolean isPropertyResettable(Object propertyId);

	Object getPropertyValue(Object propertyId);

	void resetPropertyValue(Object propertyId);

	void setPropertyValue(Object propertyId, Object unwrapValue);

	Object getEditableValue();


}
