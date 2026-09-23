000100 01  R448.                                                                
000200*                                 R448-POSTER   INGÅR I                   
000300*                                 PLOCKFREKVENSSTATISTIKEN W430           
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 SORTKOD              PIC X.                                       
000700*                                 SORTERINGSKOD                           
000800     03 KDCLAGER             PIC S9              COMP-3.                  
000900*                                 CENTRALLAGERKOD                         
001000     03 SORT1                PIC S9(9)           COMP-3.                  
001100*                                 SORTERINGSFÄLT          KDSORT9         
001200     03 SORT2                PIC S9(9).                                   
001300*                                 SORTERINGSFÄLT          KDSORT9         
001400     03 SORT3                PIC S9(9)           COMP-3.                  
001500*                                 SORTERINGSFÄLT          KDSORT9         
001600     03 IDARTNR              PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800     03 IDRADNR              PIC S9(3)           COMP-3.                  
001900*                                                     IDRADNR-002         
002000*                                 BEHANDLAD RAD INOM ORDER                
002100*                                 ANVÄNDS VID CHKPT                       
002200     03 ADLAGOMR             PIC S9(9)           COMP-3.                  
002300*                                 LAGEROMRÅDE        ADLAGOMR-003         
002400     03 KDEMBHT              PIC S9(7)           COMP-3.                  
002500*                                 HANTERINGSKOD                           
002600     03 VKARTNTO             PIC S9(7)           COMP-3.                  
002700*                                 ARTIKELVIKT        VKARTNTO-002         
002800*                                 NETTO (G)                               
002900     03 VLARTNTO             PIC S9(2)V9(7)      COMP-3.                  
003000*                                 ARTIKELVOLYM NETTO VLARTNTO-002         
003100     03 AALDER               PIC S9              COMP-3.                  
003200*                                 ÅLDER                                   
003300     03 KDERS                PIC S9(3)           COMP-3.                  
003400*                                 ERSÄTTNINGSKOD                          
003500     03 VKLAGNTO             PIC S9(7)           COMP-3.                  
003600*                                 NETTOVIKT PER LAGEROMRÅDE               
003700     03 VLLAGNTO             PIC S9(4)V9(3)      COMP-3.                  
003800*                                 NETTOVOLYM PER LAGEROMRÅDE              
003900*** END COPY W430448CC0  LENGTH=60                                        
