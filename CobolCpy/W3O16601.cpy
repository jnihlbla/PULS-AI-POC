000100 01  MOD-W3O16601.                                                        
000200*                                 COPYTEXT FÖR MOD                        
000300*                                 W3O16601                                
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDARTNR-IN       PIC X(2).                                    
000900*                                 MFS BEHANDLING AV INPUTFÄLT             
001000     03 MOD-IDARTNR-UT       PIC X(8).                                    
001100*                                 ARTIKELNUMMER                           
001200     03 MOD-OUTPUTDATA.                                                   
001300        05 MOD-BEART-CORE    PIC X(25).                                   
001400*                                 ENGELSK ARTIKELBENÄMNING                
001500        05 MOD-KVLS-INT      PIC -(6)9.                                   
001600*                                 ANTAL                                   
001700        05 MOD-KVADV-INT     PIC -(6)9.                                   
001800*                                 ANTAL                                   
001900        05 MOD-KVTOT-INT     PIC -(6)9.                                   
002000*                                 ANTAL                                   
002100        05 MOD-KVTRA-INT     PIC -(6)9.                                   
002200*                                 ANTAL                                   
002300        05 MOD-KVLS-REM      PIC -(6)9.                                   
002400*                                 ANTAL                                   
002500        05 MOD-KVADV-REM     PIC -(6)9.                                   
002600*                                 ANTAL                                   
002700        05 MOD-KVTOT-REM     PIC -(6)9.                                   
002800*                                 ANTAL                                   
002900        05 MOD-KVLS-TOT      PIC -(6)9.                                   
003000*                                 ANTAL                                   
003100        05 MOD-KVADV-TOT     PIC -(6)9.                                   
003200*                                 ANTAL                                   
003300        05 MOD-KVTOT-TOT     PIC -(6)9.                                   
003400*                                 ANTAL                                   
003500        05 MOD-KVLS-11       PIC -(6)9.                                   
003600*                                 ANTAL                                   
003700        05 MOD-KVADV-11      PIC -(6)9.                                   
003800*                                 ANTAL                                   
003900        05 MOD-KVTOT-11      PIC -(6)9.                                   
004000*                                 ANTAL                                   
004100        05 MOD-KVTRA-11      PIC -(6)9.                                   
004200*                                 ANTAL                                   
004300        05 MOD-KVLS-91       PIC -(6)9.                                   
004400*                                 ANTAL                                   
004500        05 MOD-KVADV-91      PIC -(6)9.                                   
004600*                                 ANTAL                                   
004700        05 MOD-KVTOT-91      PIC -(6)9.                                   
004800*                                 ANTAL                                   
004900        05 MOD-KVTRA-91      PIC -(6)9.                                   
005000*                                 ANTAL                                   
005100        05 MOD-NDCDATA       OCCURS 7 TIMES.                              
005200           07 MOD-DC-TYP     PIC X.                                       
005300           07 MOD-IDDC       PIC X(2).                                    
005400*                                 IDENTIFIERARE LAGER                     
005500           07 MOD-KVLS       PIC -(6)9.                                   
005600*                                 ANTAL                                   
005700           07 MOD-KVADV      PIC -(6)9.                                   
005800*                                 ANTAL                                   
005900           07 MOD-KVTOT      PIC -(6)9.                                   
006000*                                 ANTAL                                   
006100           07 MOD-KVTRA      PIC -(6)9.                                   
006200*                                 ANTAL                                   
006300     03 MOD-TEMFSINF         PIC X(55).                                   
006400*                                 INFORMATIONSMEDDELANDE                  
006500*** END OF VILMAII-COPY LENGTH= 477 BYTES                                 
