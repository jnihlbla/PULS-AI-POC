000010 01  VTAB-EMBALLAGEPOST.                                                  
000020     03   VTAB-POSTTYP              PIC 9(1).                             
000030*                                   POSTTYP KOD=1 HUVUDPOST               
000040*                                           KOD=2 RADPOST                 
000050     03   VTAB-FSEDELNR             PIC 9(7).                             
000060*                                   F÷LJESEDELNUMMER                      
000070     03   VTAB-HUVUDPOST.                                                 
000080*                                                                         
000090          05   VTAB-KONTO-FRAN      PIC 9(6).                             
000100*                                   FR≈N KONTO                            
000110          05   VTAB-IDDISTR         PIC 9(5).                             
000120*                                   DISTRIKTSNUMMER                       
000130          05   VTAB-IDKUNDNR        PIC 9(7).                             
000140*                                   KUNDNUMMER                            
000150          05   VTAB-DATUM           PIC 9(8).                             
000160*                                   DATUM ≈≈MMDD                          
000170     03   VTAB-RADPOST     REDEFINES VTAB-HUVUDPOST.                      
000180*                                                                         
000190          05   VTAB-EMBTYP          PIC 9(5).                             
000200*                                   EMBALLAGETYP                          
000201          05   FILLER               PIC X(1).                             
000210          05   VTAB-EMBANTAL        PIC 9(5).                             
000220*                                   EMBALLAGEANTAL                        
000221          05   FILLER               PIC X(15).                            
000230*** END COPY W4739202C0  LENGTH=34                                        
