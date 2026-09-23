000100 01  ART-WDK901.                                                          
000200*                                 ARTIKELREGISTER                         
000300*                                 ORDER ENTRY                             
000400*                                 FYSISK NYCKEL: IDARTNR                  
000500     03 ART-IDARTNR          PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700*                                 PART NUMBER                             
000800     03 ART-KVOFFERT         PIC S9(7)           COMP-3.                  
000900*                                 OFFERTSALDO                             
001000*                                 OFFERED QUANTITY                        
001100     03 ART-KVOKS-BULK       PIC S9(7)           COMP-3.                  
001200*                                 ORDERKÖSALDO, KLASS 2-4                 
001300*                                 ORDER QUEUE BALANCE, CLASS 2-4          
001400     03 ART-KVOKS-DAG        PIC S9(7)           COMP-3.                  
001500*                                 ORDERKÖSALDO, KLASS 1                   
001600*                                 ORDER QUEUE BALANCE, CLASS 1            
001700     03 ART-KVOKS-VOR        PIC S9(7)           COMP-3.                  
001800*                                 ORDERKÖSALDO, VOR                       
001900*                                 ORDER QUEUE BALANCE, VOR                
002000     03 ART-KVPREAVB-BULK    PIC S9(7)           COMP-3.                  
002100*                                 PREL-AVB KVANT, KLASS 2-4               
002200*                                 PREL-RES QUANT, CLASS 2-4               
002300     03 ART-KVPREAVB-DAG     PIC S9(7)           COMP-3.                  
002400*                                 PREL-AVB KVANT, KLASS 1                 
002500*                                 PREL-RES QUANT, CLASS 1                 
002600     03 ART-KVPREAVB-VOR     PIC S9(7)           COMP-3.                  
002700*                                 PREL-AVB KVANT, VOR                     
002800*                                 PREL-RES QUANT, VOR                     
002900     03 ART-KVPRERO-BULK     PIC S9(7)           COMP-3.                  
003000*                                 PRELIMINÄR RO-KVANT, KLASS 2-4          
003100*                                 PRELIMINARY BO-QUANT, CLASS 2-4         
003200     03 ART-KVPRERO-DAG      PIC S9(7)           COMP-3.                  
003300*                                 PRELIMINÄR RO-KVANT, KLASS 1            
003400*                                 PRELIMINARY BO-QUANT, CLASS 1           
003500     03 ART-RERF-ART         PIC S9V9(4)         COMP-3.                  
003600*                                 RANSONERINGSFAKTOR ARTIKEL              
003700*                                 RATIONINGFACTOR ARTICLE                 
003800     03 ART-SUTPO-TOT        PIC S9(7)           COMP-3.                  
003900*                                 TPO-KVANTITET, TOTAL                    
004000*                                 TPO-QUANTITY, TOTAL                     
004100     03 ART-FILLER           PIC X(14).                                   
004200*** END OF VILMAII-COPY LENGTH= 62 BYTES                                  
