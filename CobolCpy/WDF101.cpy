000100 01  LEV-WDF101.                                                          
000200*                                 LEVERANTÖRSREGISTER                     
000300*                                 ALLMÄN LEVERANTÖRSINFO                  
000400*                                 FYSISK NYCKEL: IDLEVNR                  
000500     03 LEV-IDLEVNR          PIC X(5).                                    
000600*                                 LEVERANTÖRNUMMER                        
000700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
000800     03 LEV-KDLEVTYP         PIC S9              COMP-3.                  
000900*                                 LEVERANTÖRTYP                           
001000     03 LEV-KDSPRAK          PIC S9              COMP-3.                  
001100*                                 SPRÅKKOD                                
001200*                                 LANGUAGE CODE                           
001300     03 LEV-FLRSADR          PIC X.                                       
001400*                                 RS-UNIK LEV-ADRESS                      
001500     03 LEV-KDGK             PIC S9              COMP-3.                  
001600*                                 GODSMOTTAGAREKOD                        
001700*                                 GOODS RECEIVING WAREHOUSE CODE          
001800     03 LEV-KVDAGAR-TTC1     PIC S9(3)           COMP-3.                  
001900*                                 DAGAR TULL- OCH TRANSPORT-TID           
002000*                                 C1                                      
002100     03 LEV-KVDAGAR-TTC2     PIC S9(3)           COMP-3.                  
002200*                                 DAGAR TULL- & TRANSPORT-TID  C2         
002300     03 LEV-KVVECKOR-LT      PIC S9(3)           COMP-3.                  
002400*                                 ANTAL VECKOR LEDTID                     
002500     03 LEV-KVVECKOR-AT      PIC S9(3)           COMP-3.                  
002600*                                 ANTAL VECKOR ANSKAFFNINGSTID            
002700     03 LEV-IDLPKOLL         PIC S9              COMP-3.                  
002800*                                 KONTROLLVECKA LEVERANSPLANER            
002900     03 LEV-PGTABELL.                                                     
003000        05 LEV-IDANSK-PG     OCCURS 8 TIMES                               
003100                             PIC S9(3)           COMP-3.                  
003200*                                 ANSKAFFARNR PER PLANERINGSGRUPP         
003300     03 LEV-LEVDAGTAB.                                                    
003400        05 LEV-TILEVDAG      OCCURS 5 TIMES                               
003500                             PIC S9              COMP-3.                  
003600*                                 AVSÄNDNINGSDAG INOM VECKA               
003700*                                 DELIVERY WEEK DAY                       
003800     03 LEV-IDLEVNR-MOTSV    PIC X(5).                                    
003900*                                 MOTSVARANDE LEVERANTÖRSID               
004000*                                 CORRESPONDING SUPPLIER ID               
004100     03 LEV-KVDAGAR-AVIAVV   PIC S9(3)           COMP-3.                  
004200*                                 TOLERANSAVVIKELSE FÖRAVISERING          
004300*                                 ACCEPTED DEVIATON PRE ADVICES           
004400     03 LEV-KVDAGAR-INLAVV   PIC S9(3)           COMP-3.                  
004500*                                 TOLERANSAVVIKELSE INLEVERANS            
004600*                                 ACCEPTED DEVIATON GOODS RECEIVI         
004700*                                 NG                                      
004800     03 LEV-DATUM-BORT       PIC S9(7)           COMP-3.                  
004900*                                 BORTTAGS-DATUM                          
005000     03 LEV-KVVECKOR-LVAR    PIC S9(2)V9(1)      COMP-3.                  
005100*                                 VARIANS I LEDTIDEN                      
005200*                                                                         
005300     03 LEV-FILLER           PIC X(8).                                    
005400*** END OF VILMAII-COPY LENGTH= 62 BYTES                                  
