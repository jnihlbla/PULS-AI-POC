000100 01  RESP-W90471O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM W90471         
000300*                                 SPIE - PROJEKTUPPFÖLJN PÅ ÄO            
000400     03 RESP-MEDDELANDE      PIC X(40).                                   
000500*                                 MFS FELMEDDELANDE                       
000600     03 RESP-IDAO-NEXT       PIC X(10).                                   
000700*                                 ÄNDRINGSORDERNUMMER                     
000800     03 RESP-IDARTNR-NEXT    PIC 9(8).                                    
000900*                                 ARTIKELNUMMER                           
001000     03 RESP-KVRADER-MAX     PIC 9(3).                                    
001100*                                 MAX INDEX KOPPLAT TILL OCCURS N         
001200*                                 EDAN.                                   
001300     03 RESP-UTDATARAD       OCCURS 1 TO 100 TIMES                        
001400                             DEPENDING ON RESP-KVRADER-MAX.               
001500*                                 ARTIKELDATA FÖR ÄO                      
001600        05 RESP-IDAO         PIC X(10).                                   
001700*                                 ÄNDRINGSORDERNUMMER                     
001800        05 RESP-IDARTNR      PIC 9(8).                                    
001900*                                 ARTIKELNUMMER                           
002000        05 RESP-TIREGDAT     PIC 9(6).                                    
002100*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002200        05 RESP-IDBERED      PIC 9(2).                                    
002300*                                 BEREDARENUMMER                          
002400        05 RESP-KDSORT       PIC X(2).                                    
002500*                                 SORT-KOD                                
002600        05 RESP-KDPRODSL     PIC 9(2).                                    
002700*                                 PRODUKTSLAG                             
002800        05 RESP-KDUART       PIC X.                                       
002900*                                 UNDANTAGSARTIKEL                        
003000        05 RESP-KDERS        PIC 9(2).                                    
003100*                                 ERSÄTTNINGSKOD                          
003200        05 RESP-IDNAMN-FORP  PIC X(40).                                   
003300*                                 NAMN                                    
003400        05 RESP-IDNAMN-ANSK  PIC X(40).                                   
003500*                                 NAMN                                    
003600        05 RESP-IDNAMN-INK   PIC X(40).                                   
003700*                                 NAMN                                    
003800        05 RESP-IDNAMN-BER   PIC X(40).                                   
003900*                                 NAMN                                    
004000        05 RESP-KDEMBKOD-EMQ2                                             
004100                             PIC X(3).                                    
004200*                                 EMBALLAGEKOD                            
004300        05 RESP-IDARTNR-KATALOG-MASTER                                    
004400                             PIC X.                                       
004500*                                 JA/NEJ-FLAGGA                           
004600        05 RESP-TIINKOP      PIC 9(6).                                    
004700*                                 DATUM NÄR INKÖP BEGÄRS                  
004800        05 RESP-TIAVTAL-FIRST                                             
004900                             PIC 9(6).                                    
005000*                                 AVTALSDATUM  (ÅÅMMDD)                   
005100        05 RESP-TILEVBSK-AVS PIC 9(5).                                    
005200*                                 ÅR - VECKA - DAG   (ÅÅVVD)              
005300        05 RESP-KVAVIS-BSKKVAR                                            
005400                             PIC 9(6).                                    
005500*                                 AVISERAT ANTAL                          
005600        05 RESP-TIAVIDAT     PIC 9(6).                                    
005700*                                 AVISERINGSDATUM (YYMMDD)                
005800        05 RESP-IDPROJ       PIC X(4).                                    
005900*                                 PARTS PROJEKTIDENTITET                  
006000        05 RESP-IDPROJK      PIC X(4).                                    
006100*                                 PROJEKTIDENTITET KONSTRUKTION           
006200        05 RESP-FLGEMFMC     PIC X.                                       
006300*                                 GEMENSAM FORD/MPNR ARTIKEL              
006400*** END OF VILMAII-COPY LENGTH= 23561 BYTES                               
