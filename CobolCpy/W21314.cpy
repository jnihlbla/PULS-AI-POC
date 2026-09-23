000100 01  W21314.                                                              
000200     03 ATGARD               PIC X(4).                                    
000300     03 IDSEGM               PIC X(6).                                    
000400*                                 SEGMENT                                 
000500     03 IDARTNR              PIC S9(9)           COMP-3.                  
000600*                                 ARTIKELNUMMER                           
000700     03 FLMANAT              PIC X.                                       
000800*                                 MANUELLT SATT ANSKAFFNINGSTID ?         
000900     03 FLMANGK              PIC X.                                       
001000*                                 FLAGGA MANUELL GODSMOTTAGARKOD          
001100     03 FLMANLT              PIC X.                                       
001200*                                 MANUELLT SATT LEDTID ?                  
001300     03 IDANSK               PIC S9(3)           COMP-3.                  
001400*                                 ANSKAFFARNUMMER                         
001500     03 IDPLANGR-AG          PIC S9              COMP-3.                  
001600*                                 PLANERINGSGRUPP ANSKAFFARE              
001700     03 KDAVT                PIC S9              COMP-3.                  
001800*                                 AVTALSMÄRKNING                          
001900     03 KDKSP                PIC S9              COMP-3.                  
002000*                                 KÖPSPÄRR                                
002100     03 KVDAGAR-TT           PIC S9(3)           COMP-3.                  
002200*                                 DAGAR TULL- OCH TRANSPORT-TID           
002300     03 WDF101.                                                           
002400*                                 LEVERANTÖRSREGISTER                     
002500*                                 ALLMÄN LEVERANTÖRSINFO                  
002600*                                 FYSISK NYCKEL: IDLEVNR                  
002700        05 IDLEVNR           PIC X(5).                                    
002800*                                 LEVERANTÖRNUMMER                        
002900        05 KDLEVTYP          PIC S9              COMP-3.                  
003000*                                 LEVERANTÖRTYP                           
003100        05 KDSPRAK           PIC S9              COMP-3.                  
003200*                                 SPRÅKKOD                                
003300        05 FLRSADR           PIC X.                                       
003400*                                 RS-UNIK LEV-ADRESS                      
003500        05 KDGK              PIC S9              COMP-3.                  
003600*                                 GODSMOTTAGAREKOD                        
003700        05 KVDAGAR-TTC1      PIC S9(3)           COMP-3.                  
003800*                                 DAGAR TULL- OCH TRANSPORT-TID           
003900*                                 C1                                      
004000        05 KVDAGAR-TTC2      PIC S9(3)           COMP-3.                  
004100*                                 DAGAR TULL- & TRANSPORT-TID  C2         
004200        05 KVVECKOR-LT       PIC S9(3)           COMP-3.                  
004300*                                 ANTAL VECKOR LEDTID                     
004400        05 KVVECKOR-AT       PIC S9(3)           COMP-3.                  
004500*                                 ANTAL VECKOR ANSKAFFNINGSTID            
004600        05 IDLPKOLL          PIC S9              COMP-3.                  
004700*                                 KONTROLLVECKA LEVERANSPLANER            
004800        05 PGTABELL.                                                      
004900           07 IDANSK-PG      OCCURS 8 TIMES                               
005000                             PIC S9(3)           COMP-3.                  
005100*                                 ANSKAFFARNR PER PLANERINGSGRUPP         
005200        05 LEVDAGTAB.                                                     
005300           07 TILEVDAG       OCCURS 5 TIMES                               
005400                             PIC S9              COMP-3.                  
005500*                                 AVSÄNDNINGSDAG INOM VECKA               
005600        05 FILLER            PIC X(6).                                    
005700     03 KDCALL               PIC S9(3)           COMP-3.                  
005800*                                 ANROPSTYP FÖR SYSTEM R2XX               
005900*** END OF VILMAII-COPY LENGTH= 72 BYTES                                  
