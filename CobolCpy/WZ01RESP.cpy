000100 01  RESP-WZ01RESP.                                                       
000200*                                 THE FIRST FIELDS IN AN RESPONSE         
000300*                                 SENT AS A RESULT OF A REQUEST           
000400*                                 FROM ONE SYSTEM COMPONENT TO            
000500*                                 ANOTHER.                                
000600     03 RESP-IDMSGVER        PIC 9(3).                                    
000700*                                 VERSIONSNUMMER PÅ MEDDELANDE            
000800*                                 VERSION NUMBER OF THE MESSAGE           
000900     03 RESP-IDMSG-INFO      PIC X(3).                                    
001000*                                 INFORMATIONSMEDDELANDE ID               
001100*                                 INFORMATION MESSAGE ID                  
001200     03 RESP-IDMSG-ERROR     PIC X(3).                                    
001300*                                 FELMEDDELANDE ID                        
001400*                                 ERROR MESSAGE ID                        
001500     03 RESP-IDELMT-ERROR    PIC X(16).                                   
001600*                                 DATAELEMENTIDENTITET                    
001700*** END OF VILMAII-COPY LENGTH= 25 BYTES                                  
