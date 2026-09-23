000010*** EDIT ALLOWED                                                          
000100 01    MSG-IO-AREA-SNUF.                                                  
000200*                                    * BESKRIVNING AV GENERELL            
000300*                                    * MID OCH MOD AREA                   
000310*                                    * VID LU0-KOMMUNIKATION              
000320*                                    * (EJ MFS, EX. TILL AS400)           
000330*                                    * OCH VID PROGRAM-TO-PROGRAM         
000340*                                    * KOMMUNIKATION                      
000400*                                    * -------------------------          
000500*                                    * DESCRIPTION OF GENERAL             
000600*                                    * MID AND MOD AREA                   
000610*                                    * USING LU0-COMMUNICATION            
000700   03    MSG-KVLL                PIC S9(4)         COMP SYNC.             
000800*                                    * MESSAGE LENGHT                     
000913   03    MSG-KDZ1                PIC X.                                   
000914*                                    * MFS FLAG POS 1                     
000919   03    MSG-KDZ2                PIC X.                                   
000920*                                    * MFS FLAG POS 2                     
000970   03    MSG-AREA.                                                        
000990*                                    * THE MESSAGE ITSELF                 
001000     05    MSG-KDTRANS.                                                   
001300*                                    * TRANSACTION CODE                   
001500       07    FILLER              PIC X(6).                                
001600       07    MSG-KDTRTYP         PIC X(1).                                
001700*                                    * TRANSACTION TYPE (U V X Y)         
002100       07    FILLER              PIC X(1).                                
002180     05    MSG-IDTRANS           PIC X(4).                                
002190*                                    * ANGER FRÅN VILKEN MID              
002200*                                    * MEDDELANDET KOMMER                 
002300*                                    * THE MID FROM WHICH THE             
002400*                                    * MESSAGE ORIGINATES                 
002500     05    MSG-KDMFSFOR          PIC X(1).                                
002600*                                    * MFS-FORMAT                         
002800*                                    * 1=SVENSKA 2=ENGLISH                
003000     05    MSG-INDATA            PIC X(6127).                             
003200*                                    * MID-INPUT                          
006960*** END COPY WMSGSNUF    LENGTH=6144  OLD LENGTH=4096                     
