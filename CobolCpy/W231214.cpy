000100 01  W231214.                                                             
000200*                                 POST PER ARTIKEL   KVANTITETER          
000300*                                 TILL OCH FRÅN LAGER                     
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDARTNR              PIC S9(9)           COMP-3.                  
000700*                                 ARTIKELNUMMER                           
000800     03 CLAGERDEL            OCCURS 2 TIMES.                              
000900        05 KVANTAL-UTLEV-12P PIC S9(9)           COMP-3.                  
001000*                                 ALLMÄNT ANTAL       KVANTAL-006         
001100        05 KVANTAL-TILL-AR   PIC S9(9)           COMP-3.                  
001200*                                 ALLMÄNT ANTAL       KVANTAL-006         
001300        05 KVANTAL-FRAN-AR   PIC S9(9)           COMP-3.                  
001400*                                 ALLMÄNT ANTAL       KVANTAL-006         
001500        05 KVANTAL-FRAN-12P  PIC S9(9)           COMP-3.                  
001600*                                 ALLMÄNT ANTAL       KVANTAL-006         
001700        05 KVANTAL-UTCLEAR   PIC S9(7)           COMP-3.                  
001800*                                 ANTAL                                   
001900        05 KVANTAL-INCLEAR   PIC S9(7)           COMP-3.                  
002000*                                 ANTAL                                   
002100        05 KVANTAL-UPPINV    PIC S9(7)           COMP-3.                  
002200*                                 ANTAL                                   
002300        05 KVANTAL-NEDINV    PIC S9(7)           COMP-3.                  
002400*                                 ANTAL                                   
002500        05 KVANTAL-PLANINL   PIC S9(7)           COMP-3.                  
002600*                                 ANTAL                                   
002700        05 KVANTAL-OPLANINL  PIC S9(7)           COMP-3.                  
002800*                                 ANTAL                                   
002900        05 KVANTAL-LAN       PIC S9(7)           COMP-3.                  
003000*                                 ANTAL                                   
003100        05 KVANTAL-RETUR     PIC S9(7)           COMP-3.                  
003200*                                 ANTAL                                   
003300        05 KVANTAL-SKROT     PIC S9(7)           COMP-3.                  
003400*                                 ANTAL                                   
003500        05 KVANTAL-K-ORDER   PIC S9(7)           COMP-3.                  
003600*                                 ANTAL                                   
003700*** END OF VILMAII-COPY LENGTH= 128 BYTES                                 
