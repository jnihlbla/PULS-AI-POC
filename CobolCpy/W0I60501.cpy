000100 01  MID-W0I60501.                                                        
000200*                                 COPYTEXT FÖR MID W0I60501               
000300     03 MID-IDPRODNR-IN      PIC X(7).                                    
000400*                                 PRODUKTIONSNUMMER                       
000500     03 MID-IDPRODNR-UT      PIC X(7).                                    
000600*                                 PRODUKTIONSNUMMER                       
000700     03 MID-IDPTYP-IN        PIC X(3).                                    
000800*                                 POSTTYP                                 
000900     03 MID-IDPTYP-UT        PIC X(3).                                    
001000*                                 POSTTYP                                 
001100     03 MID-IDKOLLI-IN       PIC X(5).                                    
001200*                                 KOLLINUMMER                             
001300     03 MID-IDKOLLI-UT       PIC X(5).                                    
001400*                                 KOLLINUMMER                             
001500     03 MID-KDTRSTAT-IN      PIC X.                                       
001600*                                 TRANSAKTIONSSTATUS                      
001700     03 MID-KDTRSTAT-UT      PIC X.                                       
001800*                                 TRANSAKTIONSSTATUS                      
001900     03 MID-KDCMDVAL         PIC X(3).                                    
002000*                                 GENERELL KOMMANDOKOD                    
002100     03 MID-IDPRODNR-GL      PIC 9(7).                                    
002200*                                 PRODUKTIONSNUMMER                       
002300     03 MID-IDPTYP-GL        PIC X(3).                                    
002400*                                 POSTTYP                                 
002500     03 MID-IDKOLLI-GL       PIC 9(5).                                    
002600*                                 KOLLINUMMER                             
002700     03 MID-KDTRSTAT-GL      PIC 9.                                       
002800*                                 TRANSAKTIONSSTATUS                      
002900     03 MID-LL-GL            PIC 9(4).                                    
003000*                                 LRECL I ETT VARIABELT RECORD            
003100     03 MID-IDPRODNR-NY      PIC 9(7).                                    
003200*                                 PRODUKTIONSNUMMER                       
003300     03 MID-IDPTYP-NY        PIC X(3).                                    
003400*                                 POSTTYP                                 
003500     03 MID-IDKOLLI-NY       PIC 9(5).                                    
003600*                                 KOLLINUMMER                             
003700     03 MID-KDTRSTAT-NY      PIC 9.                                       
003800*                                 TRANSAKTIONSSTATUS                      
003900     03 MID-LL-NY            PIC 9(4).                                    
004000*                                 LRECL I ETT VARIABELT RECORD            
004100     03 MID-LINES            OCCURS 5 TIMES.                              
004200        05 MID-TEDATA-GL     PIC X(70).                                   
004300        05 MID-TEDATA-NY     PIC X(70).                                   
004400*** END COPY W0I60501C0  LENGTH=775                                       
