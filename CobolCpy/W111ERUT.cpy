000100 01  RESP-W111ERUT.                                                       
000200     03 RESP-KDSVAR          PIC X.                                       
000300*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
000400     03 RESP-TEMFSFEL        PIC X(40).                                   
000500*                                 MFS FELMEDDELANDE                       
000600     03 RESP-IDKORTNR-SPAR1  PIC X(3).                                    
000700*                                 KORTNUMMER                              
000800*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
000900     03 RESP-IDKORTNR-SPAR2  PIC X(3).                                    
001000*                                 KORTNUMMER                              
001100*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
001200     03 RESP-IDKORTNR-SPAR3  PIC X(3).                                    
001300*                                 KORTNUMMER                              
001400*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
001500     03 RESP-DIERS-ERS-ATTR  PIC X(2).                                    
001600*                                 MFS ATTRIBUTFÄLT                        
001700     03 RESP-DIERS-ERS       PIC X(7).                                    
001800*                                 KVANTITET I ERSÄTTN.                    
001900     03 RESP-KDERS-ATTR      PIC X(2).                                    
002000*                                 MFS ATTRIBUTFÄLT                        
002100     03 RESP-KDERS           PIC X(2).                                    
002200*                                 ERSÄTTNINGSKOD                          
002300     03 RESP-IDAO-ATTR       PIC X(2).                                    
002400*                                 MFS ATTRIBUTFÄLT                        
002500     03 RESP-IDAO            PIC X(10).                                   
002600*                                 ÄNDRINGSORDERNUMMER                     
002700     03 RESP-TIERSDAT-PREL-ATTR                                           
002800                             PIC X(2).                                    
002900*                                 MFS ATTRIBUTFÄLT                        
003000     03 RESP-TIERSDAT-PREL   PIC X(5).                                    
003100*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
003200     03 RESP-TEARTNOT-ATTR   PIC X(2).                                    
003300*                                 MFS ATTRIBUTFÄLT                        
003400     03 RESP-TEARTNOT        PIC X(40).                                   
003500*                                 ARTIKEL NOTERING                        
003600     03 RESP-IDUSER          PIC X(8).                                    
003700*                                 ANVÄNDARENS SÄKERHETS ID                
003800     03 RESP-FLKLAR-ATTR     PIC X(2).                                    
003900*                                 MFS ATTRIBUTFÄLT                        
004000     03 RESP-FLKLAR          PIC X.                                       
004100*                                 AVSLUTNINGSMARKERING                    
004200     03 RESP-KDERS-C1        PIC X(2).                                    
004300*                                 ERSÄTTNINGSKOD                          
004400     03 RESP-KDERS-C2        PIC X(2).                                    
004500*                                 ERSÄTTNINGSKOD                          
004600     03 RESP-TIERSDAT-REG    PIC X(5).                                    
004700*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
004800     03 RESP-TIERSDAT-PREL-C1                                             
004900                             PIC X(5).                                    
005000*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
005100     03 RESP-TIERSDAT-PREL-C2                                             
005200                             PIC X(5).                                    
005300*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
005400     03 RESP-TIERSDAT        PIC X(5).                                    
005500*                                 ERSÄTTNINGSDATUM  (ÅÅVVD)               
005600     03 RESP-TEMFSINF        PIC X(55).                                   
005700*                                 INFORMATIONSMEDDELANDE                  
005800     03 RESP-FELTEXT         PIC X(30).                                   
005900     03 RESP-KVRADER-MAX9    PIC 9(5).                                    
006000*                                 MAX INDEX KOPPLAT TILL OCCURS N         
006100*                                 EDAN.                                   
006200     03 RESP-UTRAD           OCCURS 1 TO 99 TIMES                         
006300                             DEPENDING ON RESP-KVRADER-MAX9.              
006400        05 RESP-IDKORTNR-ATTR                                             
006500                             PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700        05 RESP-IDKORTNR     PIC X(3).                                    
006800*                                 KORTNUMMER                              
006900*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
007000        05 RESP-FLTEXT       PIC X.                                       
007100*                                 FINNS TEXTINFORMATION ?                 
007200        05 RESP-IDARTNR-TILLK-ATTR                                        
007300                             PIC X(2).                                    
007400*                                 MFS ATTRIBUTFÄLT                        
007500        05 RESP-IDARTNR-TILLK                                             
007600                             PIC X(9).                                    
007700*                                 ARTIKELNUMMER                           
007800        05 RESP-DIERS-TILLK-ATTR                                          
007900                             PIC X(2).                                    
008000*                                 MFS ATTRIBUTFÄLT                        
008100        05 RESP-DIERS-TILLK  PIC X(7).                                    
008200*                                 KVANTITET I ERSÄTTN.                    
008300        05 RESP-BEART        PIC X(25).                                   
008400*                                 ARTIKELBENÄMNING                        
008500        05 RESP-BEERS-ATTR   PIC X(2).                                    
008600*                                 MFS ATTRIBUTFÄLT                        
008700        05 RESP-BEERS        PIC X(20).                                   
008800*                                 ERSÄTTNINGSTEXT                         
008900*** END OF VILMAII-COPY LENGTH= 7476 BYTES                                
