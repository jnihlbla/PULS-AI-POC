000010*** EDIT ALLOWED                                                          
000100 01  W475B802.                                                            
000200*                                                                         
000300*   LAYOUT OF F2-TRANSACTION FOR FABRY                                    
000400*                                                                         
000500     03  IDPTYP          PIC X(2)   VALUE 'F2'.                           
000600*                TRANSACTION TYPE                                         
000700     03  IDFAKT          PIC 9(7).                                        
000800*                INVOICE NUMBER                                           
000900     03  KDCLAGER        PIC 9(1).                                        
001000*                WAREHOUSE CODE                                           
001100*                1 = SUEDE                                                
001200*                2 = BELGIQUE                                             
001300     03  KVCOLLI         PIC 9(3).                                        
001400*                NUMBER OF CASES FOR THIS INVOICE                         
001500     03  VKORDNTO        PIC 9(6)V9.                                      
001600*                TOTAL NET WEIGHT FOR THIS INVOICE                        
001700     03  VKORDBTO        PIC 9(6)V9.                                      
001800*                TOTAL GROSS WEIGHT FOR THIS INVOICE                      
001900     03  SUFKTUTL        PIC 9(9)V99.                                     
002000*                INVOICE VALUE IN FRF                                     
002100     03  NATCONTR        PIC 9(2)    VALUE 20.                            
002200*                NATURE OF CONTROL                                        
002300*                20 = FOB                                                 
002400*                28 = CIF SEULEMENT TRANSPORT                             
002500*                29 = TOUT FRAIS PAYE PAR VOLVO GENT                      
002600     03  PAYRULES        PIC 9(2)    VALUE 1.                             
002700*                MODE DE PAIEMENT                                         
002800*                01 = 30 JOURS                                            
002900*                02 = 31 A 60 JOURS                                       
003000*                03 = 61 A 90 JOURS                                       
003100     03  FILLER          PIC X(38)   VALUE SPACES.                        
003200*                                                                         
003300*** END COPY W475B802    LENGTH=80                                        
