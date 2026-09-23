000010*** EDIT ALLOWED                                                          
000100 01  DATUMKORT.                                                           
000200*                          DATUMKORT FÖR RS NATTBATCH                     
000300*                          ID = RC2001                                    
000400         03  DATUM             PIC X(5)    DISPLAY.                       
000500*                          VÄRDE:  'DATE9'          KOL: 01-05            
000600         03  FAELT2            PIC X(6).                                  
000700*                          IDDEL = 'RC2001'         KOL: 06-11            
000800         03  D-AAR             PIC 9(2).                                  
000900*                          ÅR  (DAGENS DATUM)       KOL: 12-13            
001000         03  D-MAANAD          PIC 9(2).                                  
001100*                          MÅNAD  (DAGENS DATUM)    KOL: 14-15            
001200         03  D-DAG             PIC 9(2).                                  
001300*                          DAG  (DAGENS DATUM)      KOL: 16-17            
001400         03  D-VECKA           PIC 9(2).                                  
001500*                          VECKONR (DAGENS DATUM)   KOL: 18-19            
001600         03  D-DAGNR           PIC 9.                                     
001700*                          DAG I VECKAN (DAGENS DATUM) KOL: 20            
001800         03  D-PERIOD          PIC 9.                                     
001900*                          PERIODNR (DAGENS DATUM)     KOL: 21            
002000         03  FILLER            PIC 9(2).                                  
002100*                          64-DEL                   KOL: 22-23            
002200         03  K-AAR             PIC 9.                                     
002300*                          KÖRNINGS-ÅR                 KOL: 24            
002400         03  K-VECKA           PIC 9(2).                                  
002500*                          KÖRNINGS-VECKONR         KOL: 25-26            
002600         03  K-PERIOD          PIC 9.                                     
002700*                          KÖRNINGS-PERIODNR           KOL: 27            
002800         03  K-STATVECKA       PIC 9.                                     
002900*                          KÖRNINGS-STATISTIKVECKA     KOL: 28            
003000         03  K-NYPER           PIC 9.                                     
003100*                          PERIODSTART? (1 = JA)       KOL: 29            
003200         03  K-PERSLUT         PIC 9.                                     
003300*                          PERIODSLUT? (1 = JA)        KOL: 30            
003400         03  K-VECKASLUT       PIC 9.                                     
003500*                          VECKOSLUT?  (1 = JA)        KOL: 31            
003600         03  FILLER            PIC X(30).                                 
003700*                                                                         
003800         03  K-VECKOSTART      PIC X.                                     
003900*                          VECKOSTART?    1 = JA       KOL: 62            
004000*                                       ÖVR = NEJ                         
004100         03  K-MANADSLUT       PIC X.                                     
004200*                          MÅNADSLUT?     1 = JA       KOL: 63            
004300*                                       ÖVR = NEJ                         
004400         03  K-PERSLUT-BOKF    PIC X.                                     
004500*                          BOKFÖRINGS-    1 = JA                          
004600*                          PERIODSLUT?  ÖVR = NEJ      KOL: 64            
004700         03  FILLER            PIC X(11).                                 
004800*                                                                         
004900         03  D-IOCSDAT         PIC 9(5).                                  
005000*                          DAGENS DAGNR                KOL: 76-80         
005100*** END COPY DATUMRC2C0  LENGTH=80    OLD LENGTH=80                       
