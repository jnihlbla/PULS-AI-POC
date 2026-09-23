000100 01  OHUV-W460007.                                                        
000200*                                 ORDERHUVUD-TRANSAKTIONER                
000300*                                 POSTTYP = RHA  NOAC-DO                  
000400     03 OHUV-SORT-IDDISTR    PIC 9(4).                                    
000500*                                 DISTRIKTNUMMER                          
000600     03 OHUV-SORT-TIFILDAT   PIC 9(6).                                    
000700*                                 DATUM NÄR EN FIL SKAPATS ÅÅMMDD         
000800     03 OHUV-SORT-TIHHMMSS   PIC 9(6).                                    
000900*                                 TIM - MIN - SEK   (HHMMSS)              
001000     03 OHUV-SORT-IDLOPNR-FIL                                             
001100                             PIC 9(5).                                    
001200*                                 TRANSAKTIONS-LÖPNUMMER                  
001300     03 OHUV-SORT-IDLOPNR    PIC 9(5).                                    
001400*                                 TRANSAKTIONS-LÖPNUMMER                  
001500     03 OHUV-IDPTYP          PIC X(3).                                    
001600*                                 POSTTYP                                 
001700     03 OHUV-IDDISTR         PIC 9(4).                                    
001800*                                 DISTRIKTNUMMER                          
001900     03 OHUV-IDKUNDNR        PIC 9(6).                                    
002000*                                 KUNDNUMMER                              
002100     03 OHUV-IDORDNR         PIC 9(7).                                    
002200*                                 ORDERNR             IDORDNR-002         
002300     03 OHUV-BEVOLREF        PIC X(10).                                   
002400*                                 VOLVO REFERENS                          
002500     03 OHUV-KDFRAKT         PIC 9(2).                                    
002600*                                 FRAKTSÄTT C1-C2 TILL KUND               
002700     03 OHUV-KDORDKL         PIC 9.                                       
002800*                                 ORDERKLASS                              
002900     03 OHUV-KDORDKL-IMP     PIC 9.                                       
003000*                                 ORDERKLASS FRÅN IMPORTÖREN              
003100     03 OHUV-KDROPACK        PIC S9.                                      
003200*                                 BIPACKNINGSINSTRUKTION RO/DO            
003300     03 OHUV-KDSPRAK         PIC 9.                                       
003400*                                 SPRÅKKOD                                
003500     03 OHUV-BEVARREF        PIC X(10).                                   
003600*                                 VÅR REFERENS                            
003700     03 OHUV-FLRESTN         PIC X.                                       
003800*                                 RESTNOTERING ?                          
003900     03 OHUV-KDNCNOT         PIC X(2).                                    
004000*                                 NOTERINGSKOD ORDER IN                   
004100     03 FILLER               PIC X(2).                                    
004200     03 OHUV-KDFEL           PIC 9(3).                                    
004300*                                 FELKOD                                  
004400*** END COPY W460007CC0  LENGTH=80                                        
