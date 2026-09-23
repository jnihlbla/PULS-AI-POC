000100 01  R449.                                                                
000200*                                 R449 POSTER   INGÅR I                   
000300*                                 PLOCKFREKVENSSTATISTIKEN W430           
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 SORTKOD              PIC X.                                       
000700*                                 SORTERINGSKOD                           
000800     03 KDCLAGER             PIC S9              COMP-3.                  
000900*                                 CENTRALLAGERKOD                         
001000     03 SORT1                PIC S9(9)           COMP-3.                  
001100*                                 SORTERINGSFÄLT          KDSORT9         
001200     03 SORT2                PIC S9(9)           COMP-3.                  
001300*                                 SORTERINGSFÄLT          KDSORT9         
001400     03 SORT3                PIC S9(9)           COMP-3.                  
001500*                                 SORTERINGSFÄLT          KDSORT9         
001600     03 IDARTNR              PIC S9(9)           COMP-3.                  
001700*                                 ARTIKELNUMMER                           
001800     03 KVRADER              PIC S9(3)           COMP-3.                  
001900*                                 ANTAL RADER         KVRADER-003         
002000     03 KVQPACK              PIC S9(5)           COMP-3.                  
002100*                                 ANTAL KVANTITETFÖRPACKNINGAR            
002200     03 KVPLOCK-KLASS-0-2    PIC S9(8)V9(3)      COMP-3.                  
002300*                                 PLOCKAD KVANTITET PER KLASS             
002400     03 KVPLOCK-KLASS-4      PIC S9(8)V9(3)      COMP-3.                  
002500*                                 PLOCKAD KVANTITET PER KLASS             
002600     03 KVPLOCK-KLASS-5      PIC S9(8)V9(3)      COMP-3.                  
002700*                                 PLOCKAD KVANTITET PER KLASS             
002800     03 PERIODV              PIC S9(8)V9(3)      COMP-3.                  
002900*                                 PERIODVÄRDE                             
003000     03 TIPER                PIC S9(8)V9(3)      COMP-3.                  
003100*                                 PERIOD                TIPER-003         
003200*** END COPY W430449CC0  LENGTH=60                                        
