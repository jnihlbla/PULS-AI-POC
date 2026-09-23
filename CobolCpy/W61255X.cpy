000100 01  W61255X.                                                             
000200*                                 STOCKTAKING HISTORY TO DATALAKE         
000300     03 INVA-IDARTNR         PIC 9(9).                                    
000400*                                 ARTIKELNUMMER                           
000500*                                 PART NUMBER                             
000600     03 INVH-IDDC            PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 INVH-DAREGDAT-CLO    PIC 9(8).                                    
001000*                                 KLARDATUM FÖR INVENTERING               
001100*                                 CLOSED DATE OF STOCK-TAKING             
001200     03 INVH-FLAUTLSJ        PIC X.                                       
001300*                                 AUTOMATISK SALDOJUSTERING               
001400*                                 AUTOMATIC STOCK BALANCE ADJUST.         
001500     03 INVH-IDUSER-CLO      PIC X(8).                                    
001600*                                 ANVÄNDAR-ID VID AVSLUT                  
001700*                                 USER ID AT CLOSING                      
001800     03 INVH-KDJUSTYP        PIC 9.                                       
001900*                                 JUSTERINGSTYP                           
002000     03 INVH-KVJUSTKV        PIC -(7)9.                                   
002100*                                 JUSTERAD KVANTITET                      
002200*                                 ADJUSTED QUANTITY                       
002300     03 INVH-PRARTSTD        PIC Z(6)9.9(2).                              
002400*                                 ARTIKELSTANDARDPRIS                     
002500*                                 STANDARD PRICE                          
002600     03 INVH-DAREGDAT-CRE    PIC 9(8).                                    
002700*                                 DATUM NÄR INVENTERING PÅBÖRJAS          
002800*                                 CREATION DATE                           
002900     03 INVH-DAREGDAT-PR1    PIC 9(8).                                    
003000*                                 DATUM FÖR FÖRSTA PRINTNING              
003100*                                 DATE FOR FIRST PRINTING                 
003200     03 INVH-DAREGDAT-PR2    PIC 9(8).                                    
003300*                                 DATUM FÖR ANDRA  PRINTNING              
003400*                                 DATE FOR SECOND PRINT                   
003500     03 INVH-DAREGDAT-PR3    PIC 9(8).                                    
003600*                                 DATUM FÖR TREDJE PRINTNING              
003700*                                 DATE FOR THIRD PRINT                    
003800     03 INVH-IDUSER-PR1      PIC X(8).                                    
003900*                                 ANVÄNDAR-ID PRINTN. 1                   
004000*                                 USER ID FIRST PRINTING                  
004100     03 INVH-IDUSER-PR2      PIC X(8).                                    
004200*                                 ANVÄNDAR-ID PRINTN. 2                   
004300*                                 USER ID SECOND PRINTING                 
004400     03 INVH-IDUSER-PR3      PIC X(8).                                    
004500*                                 ANVÄNDAR-ID PRINTN. 3                   
004600*                                 USER ID THIRD PRINTING                  
004700     03 INVH-IDUSER-CRE      PIC X(8).                                    
004800*                                 ANVÄNDAR-ID VID START                   
004900*                                 USER ID AT START                        
005000     03 INVH-KVANTAL         PIC Z(7).                                    
005100*                                 ANTAL                                   
005200*                                 NUMBER                                  
005300*** END OF VILMAII-COPY LENGTH= 118 BYTES                                 
