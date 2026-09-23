000100 01  MOD-W4O90301.                                                        
000200*                                 COPYTEXT FÖR RO-RELEASE 2               
000300*                                                                         
000400     03 MOD-IDTRANS          PIC X(4).                                    
000500*                                 BILDNUMMER                              
000600     03 MOD-TEMFSFEL         PIC X(40).                                   
000700*                                 MFS FELMEDDELANDE                       
000800     03 MOD-IDDISTR-IN       PIC X(4).                                    
000900*                                 DISTRIKTNUMMER                          
001000     03 MOD-IDDISTR-UT       PIC X(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001300*                                 KUNDNUMMER                              
001400     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001500*                                 KUNDNUMMER                              
001600     03 MOD-KDFRAKT-IN       PIC X(2).                                    
001700*                                 FRAKTSÄTT C1-C2 TILL KUND               
001800     03 MOD-KDFRAKT-UT       PIC X(2).                                    
001900*                                 FRAKTSÄTT C1-C2 TILL KUND               
002000     03 MOD-KDORDKL-IN       PIC X.                                       
002100*                                 ORDERKLASS                              
002200     03 MOD-KDORDKL-UT       PIC X.                                       
002300*                                 ORDERKLASS                              
002400     03 MOD-KDFRAKT-MIN      PIC X(2).                                    
002500*                                 FRAKTSÄTT C1-C2 TILL KUND               
002600     03 MOD-KDORDKL-MIN      PIC X.                                       
002700*                                 ORDERKLASS                              
002800     03 MOD-KDFRAKT-MAX      PIC X(2).                                    
002900*                                 FRAKTSÄTT C1-C2 TILL KUND               
003000     03 MOD-KDORDKL-MAX      PIC X.                                       
003100*                                 ORDERKLASS                              
003200     03 MOD-RAD-INFO         OCCURS 14 TIMES.                             
003300*                                                                         
003400        05 MOD-IDKUNDNR      PIC X(6).                                    
003500*                                 KUNDNUMMER                              
003600        05 FILLER            PIC X(4).                                    
003700        05 MOD-KDFRAKT       PIC Z(2).                                    
003800*                                 FRAKTSÄTT C1-C2 TILL KUND               
003900        05 FILLER            PIC X(6).                                    
004000        05 MOD-KDORDKL       PIC Z.                                       
004100*                                 ORDERKLASS                              
004200        05 FILLER            PIC X(7).                                    
004300        05 MOD-KDROPACK      PIC X.                                       
004400*                                 BIPACKNINGSINSTRUKTION RO/DO            
004500        05 FILLER            PIC X(7).                                    
004600        05 MOD-TISTADAT      PIC 9(6).                                    
004700*                                 STARTDATUM                              
004800        05 FILLER            PIC X(4).                                    
004900        05 MOD-EVEN-DAYS     OCCURS 5 TIMES.                              
005000           07 MOD-TID-EVEN   PIC X.                                       
005100*                                 DAGNUMMER I VECKA (MÅN = 1)             
005200           07 FILLER         PIC X.                                       
005300        05 FILLER            PIC X(3).                                    
005400        05 MOD-ODD-DAYS      OCCURS 5 TIMES.                              
005500           07 MOD-TID-ODD    PIC X.                                       
005600*                                 DAGNUMMER I VECKA (MÅN = 1)             
005700           07 FILLER         PIC X.                                       
005800     03 MOD-TEMFSINF         PIC X(61).                                   
005900*                                 INFORMATIONSMEDDELANDE                  
006000*** END COPY W4O90301C0  LENGTH=1075                                      
