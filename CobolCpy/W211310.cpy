000100 01  W211310.                                                             
000200*                                 POSTTYP 310                             
000300*                                                                         
000400     03 IDTTYP               PIC X(3).                                    
000500*                                 TRANSAKTIONSTYP                         
000600     03 IDARTNR-S            PIC 9(8).                                    
000700*                                 ARTIKELNUMMER                           
000800     03 KDCLAGER-S           PIC 9.                                       
000900*                                 CENTRALLAGERKOD                         
001000     03 SORTFLT1             PIC 9(2).                                    
001100*                                 SORTERINGSFÄLT                          
001200     03 IDANSKNR             PIC 9(3).                                    
001300*                                 ANSKAFFARNUMMER                         
001400     03 PRARTSTD             PIC 9(7)V9(2).                               
001500*                                 ARTIKELSTANDARDPRIS                     
001600     03 IDLOPNR              PIC 9(8).                                    
001700*                                 LÖPNUMMER           IDLOPNR-003         
001800     03 KDAVVANT             PIC 9.                                       
001900*                                 AVVIKELSEANTAL KOD                      
002000*                                 0=INGEN ANM.   1=AVVIKELSE              
002100*                                 2=MAKULERING AV MOTT.RAPPORT            
002200     03 POSTLGD              PIC 9(3).                                    
002300     03 IDPTYP               PIC 9(3).                                    
002400*                                 INLEVERANS-POSTTYP                      
002500     03 NOLLOR-20            PIC 9(2).                                    
002600     03 IDARTNR              PIC 9(8).                                    
002700*                                 ARTIKELNUMMER                           
002800     03 KDCLAGER             PIC 9.                                       
002900*                                 CENTRALLAGERKOD                         
003000     03 KVMOTANT             PIC S9(7).                                   
003100*                                 ANTAL MOTTAGET                          
003200     03 NOLLOR-41            PIC 9(4).                                    
003300     03 IDLEVNR-INL          PIC X(5).                                    
003400*                                 LEVERANTÖR FÖR AKTUELL INLEV.           
003500     03 TIAVSDAT             PIC 9(6).                                    
003600*                                 AVISERINGSDATUM (YYMMDD)                
003700     03 NOLLOR-21            PIC 9(2).                                    
003800     03 KDRT                 PIC 9(2).                                    
003900*                                 REDOVISNINGSTYP                         
004000     03 KVAVIS               PIC S9(7).                                   
004100*                                 AVISERAT ANTAL                          
004200     03 IDAVINR              PIC 9(7).                                    
004300*                                 AVI-NUMMER                              
004400     03 KDPKINR              PIC 9(3).                                    
004500*** END OF VILMAII-COPY LENGTH= 95 BYTES                                  
