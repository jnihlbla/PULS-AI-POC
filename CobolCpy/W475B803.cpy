000010*** EDIT ALLOWED                                                          
000100 01  W475B803.                                                            
000200*                                                                         
000300*   LAYOUT OF F3-TRANSACTION FOR FABRY                                    
000400*                                                                         
000500     03  IDPTYP          PIC X(2)   VALUE 'F3'.                           
000600*                TRANSACTION TYPE                                         
000700     03  IDFAKT          PIC 9(7).                                        
000800*                INVOICE NUMBER                                           
000900     03  KDARTURS-F      PIC X(2).                                        
001000*                ORIGIN CODE FOR FRANCE                                   
001100     03  IDSTATNR-F      PIC X(13).                                       
001200*                FRENCH STATISTICAL NUMBER                                
001300     03  SUVIKTNTO       PIC 9(5)V999.                                    
001400*                NET WEIGHT OF ORDER LINE                                 
001500     03  SUVIKTBTO       PIC 9(5)V999.                                    
001600*                GROSS WEIGHT OF ORDER LINE                               
001700     03  KVLEVART        PIC 9(6).                                        
001800*                QUANTITY OF ORDER LINE                                   
001900     03  SUFRFLINE       PIC 9(6)V99.                                     
002000*                        VALUE IN FRF OF ORDER LINE                       
002100     03  FILLER          PIC X(25)   VALUE SPACES.                        
002200*                                                                         
002300*** END COPY W475B803    LENGTH=80                                        
