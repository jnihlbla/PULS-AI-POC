000100 01  W092FREG.                                                            
000200     03 SORTDEL.                                                          
000300        05 IDNYCKEL.                                                      
000400*                                 NYCKEL FÖR DIREKT-ADRESSERING           
000500           07 IDKTYP         PIC X(3).                                    
000600*                                 INPUT-TRANSAKTIONENS TRANSTYP           
000700           07 IDFELKODX      PIC X(3).                                    
000800*                                 FELKOD                                  
000900        05 IDSYSTEM          PIC X(4).                                    
001000*                                 SKAPANDE SYSTEMNUMMER                   
001100        05 KDLISTAD          PIC X(2).                                    
001200*                                 LISTADRESS                              
001300        05 KDLISTSO          PIC 9(2).                                    
001400*                                 LISTSORTERING                           
001500        05 KDLISTAD-S        PIC X(2).                                    
001600*                                 LISTADRESS SEKUNDÄR                     
001700        05 KDLISTSO-S        PIC 9(2).                                    
001800*                                 LISTSORTERING SEKUNDÄR                  
001900     03 KDSVAR               PIC X.                                       
002000*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
002100     03 FLFELMED             PIC X.                                       
002200*                                 R = TEXT HÄMTAS FRÅN REGISTER           
002300*                                 T = TEXT HÄMTAS FRÅN TRANS              
002400*                                 I = TEXT HÄMTAS FRÅN TRANS              
002500*                                                       INFO-MED          
002600     03 BEFELTXT-SV          PIC X(70).                                   
002700*                                 SVENSK FELTEXT                          
002800     03 BEFELTXT-ENG         PIC X(70).                                   
002900*                                 ENGELSK FELTEXT                         
003000*** END COPY W092FREG    LENGTH=160                                       
