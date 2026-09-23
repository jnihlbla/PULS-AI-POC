000100 01  W4797402.                                                            
000200*                                 PACKUNDERLAG, HISTORIK                  
000300*                                 KOPPLINGSDATUM TILL LEV.ANM.            
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDDISTR              PIC S9(5)           COMP-3.                  
000700*                                 DISTRIKTNUMMER                          
000800     03 IDKUNDNR             PIC S9(7)           COMP-3.                  
000900*                                 KUNDNUMMER                              
001000     03 IDPRODNR             PIC S9(7)           COMP-3.                  
001100*                                 PRODUKTIONSNUMMER                       
001200     03 IDDC                 PIC X(2).                                    
001300*                                 IDENTIFIERARE LAGER                     
001400     03 IDKOLLI              PIC S9(5)           COMP-3.                  
001500*                                 KOLLINUMMER                             
001600     03 IDPURAD              PIC S9(5)           COMP-3.                  
001700*                                 RADNUMMER P≈ PACKUNDERLAG               
001800     03 IDKUNDRF             PIC X(10).                                   
001900*                                 KUNDENS REFERENS (ORDERID)              
002000     03 TIFAKT               PIC S9(7)           COMP-3.                  
002100*                                 FAKTURERINGSDATUM (≈≈MMDD)              
002200*** END COPY W4797402    LENGTH=36                                        
