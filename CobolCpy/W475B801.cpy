000010*** EDIT ALLOWED                                                          
000100 01  W475B801.                                                            
000200*                                                                         
000300*   LAYOUT OF F1-TRANSACTION FOR FABRY                                    
000400*                                                                         
000500     03  IDPTYP          PIC X(2)   VALUE 'F1'.                           
000600*                TRANSACTION TYPE                                         
000700     03  FILLER          PIC X(1)   VALUE SPACE.                          
000800*                FOR SORT                                                 
000900     03  TIXMIT          PIC 9(6).                                        
001000*                TRANSMISSION-DATE YYMMDD                                 
001100     03  IDFABRY         PIC X(6)   VALUE '032277'.                       
001200*                IDENTIFICATION VOLVO IN FABRY                            
001300     03  IDLBBET-C2      PIC X(6).                                        
001400*                TRAILERNUMBER      , BVT NUMBER                          
001500     03  NATTRUCK        PIC 9(3)     VALUE 2.                            
001600*                COUNTRY IDENTIFICATION OF TRUCK                          
001700     03  IDLORREG        PIC X(6).                                        
001800*                LORRY REGISTRATION NUMBER                                
001900     03  KDTDOC          PIC X(2).                                        
002000*                TRANSPORT DOCUMENT TYPE                                  
002100     03  IDTDOC          PIC 9(7).                                        
002200*                TRANSPORT DOCUMENT NUMBER                                
002300     03  KVFAKT          PIC 9(3).                                        
002400*                NUMBER OF INVOICES TO BE TRANSMITTED                     
002500     03  FILLER          PIC X(38)    VALUE SPACES.                       
002600*                                                                         
002700*** END COPY W475B801    LENGTH=80    OLD LENGTH=                         
