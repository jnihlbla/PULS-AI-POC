000100 01  W11410.                                                              
000200*                                 COPYTEXT FÖR FILEN PC3248F1             
000300*                                 FRÅN KDP.   ( MPNR )                    
000400     03 IDPTYP               PIC X.                                       
000500*                                 MPNR POSTTYP                            
000600     03 IDPRTNER-1           PIC 9(5).                                    
000700*                                 PARTNER-ID  P.A.G.                      
000800     03 KDPRTNER-1           PIC X(4).                                    
000900*                                 PARTNERKOD  P.A.G.                      
001000     03 IDARTMPNR            PIC X(50).                                   
001100*                                 MPNR ARTIKELIDENTITET                   
001200     03 IDARTFMC-FILLER REDEFINES IDARTMPNR.                              
001300        05 IDARTFMC.                                                      
001400*                                 FORD ARTIKELIDENTITET KONSTR.           
001500           07 IDARTPFX       PIC X(6).                                    
001600*                                 ARTIKELPREFIX FORD                      
001700           07 IDARTBAS       PIC X(8).                                    
001800*                                 BASARTIKELNUMMER FORD                   
001900           07 IDARTSFX       PIC X(8).                                    
002000*                                 FORD ARTIKELNUMMER SUFFIX               
002100        05 FILLER            PIC X(28).                                   
002200     03 BEARTFMC-FORD        PIC X(34).                                   
002300*                                 ARTIKELBENÄMNING FORD                   
002400     03 IDPRTNER-2           PIC 9(5).                                    
002500*                                 PARTNER-ID  P.A.G.                      
002600     03 KDPRTNER-2           PIC X(4).                                    
002700*                                 PARTNERKOD  P.A.G.                      
002800     03 IDARTMPNR            PIC X(50).                                   
002900*                                 MPNR ARTIKELIDENTITET                   
003000     03 IDARTVOL-FILLER REDEFINES IDARTMPNR.                              
003100        05 IDARTVOL.                                                      
003200*                                 VOLVO KDP ARTIKELIDENTITET              
003300           07 IDARTNR        PIC X(8).                                    
003400*                                 ARTIKELNUMMER                           
003500           07 KDARTUTF       PIC X(3).                                    
003600*                                 ARTIKELUTF. VOLVO-KDP                   
003700        05 FILLER            PIC X(39).                                   
003800     03 BEARTFMC-KDP         PIC X(34).                                   
003900*                                 ARTIKELBENÄMNING KDP                    
004000     03 IDPRTNER-OWNER       PIC 9(5).                                    
004100*                                 OWNER PARTNER-ID  P.A.G.                
004200     03 FLGEMFMC             PIC X.                                       
004300*                                 GEMENSAM FORD/MPNR ARTIKEL              
004400     03 BETEXT-FMC           PIC X(20).                                   
004500*                                 KOMMENTAR FORD                          
004600     03 BETEXT-KDP           PIC X(20).                                   
004700*                                 KOMMENTAR KDP                           
004800     03 IDCDS                PIC X(8).                                    
004900*                                 ANVÄNDARENS CDS ID                      
005000     03 DAREGFMC             PIC X(10).                                   
005100*                                 REG.DATUM FORD (ÅÅÅÅ-MM-DD)             
005200     03 TIREGFMC             PIC X(8).                                    
005300*                                 REGISTRERINGSTID, FORD                  
005400     03 FILLER               PIC X(41).                                   
005500*** END OF VILMAII-COPY LENGTH= 300 BYTES                                 
