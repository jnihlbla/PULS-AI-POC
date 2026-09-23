000100 01  RESP-WL0186O1.                                                       
000200*                                 RESPONS FROM PGM WL0186                 
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDTRPTNR-KEY    PIC 9(3).                                    
000600*                                 TRANSPORTIDENTITET                      
000700     03 RESP-IDLBBET-KEY     PIC X(12).                                   
000800*                                 LASTBÄRARBETECKNING                     
000900     03 RESP-KDFARLIG-KEY    PIC X.                                       
001000*                                 KOD FÖR FARLIGT GODS                    
001100     03 RESP-TIRFSDAT-KEY    PIC 9(6).                                    
001200*                                 KLART FÖR TRANSPORT ÅÅMMDD              
001300     03 RESP-SUMMOR.                                                      
001400*                                                                         
001500        05 RESP-KVKOLLI-TOT  PIC 9(4).                                    
001600*                                 ANTAL KOLLI                             
001700        05 RESP-VKORDBTO-TOT PIC 9(7)V9(1).                               
001800*                                 ORDERVIKT BRUTTO (KG)                   
001900        05 RESP-VLORDBTO-TOT PIC 9(5)V9(3).                               
002000*                                 ORDERVOLYM BRUTTO (M3)                  
002100        05 RESP-KVKOLLI-VALD PIC 9(4).                                    
002200*                                 ANTAL KOLLI                             
002300        05 RESP-VKORDBTO-VALD                                             
002400                             PIC 9(6)V9(1).                               
002500*                                 ORDERVIKT BRUTTO (KG)                   
002600        05 RESP-VLORDBTO-VALD                                             
002700                             PIC 9(4)V9(3).                               
002800*                                 ORDERVOLYM BRUTTO (M3)                  
002900     03 RESP-FLSIDLAST       PIC X.                                       
003000*                                 LASTA HEL SIDA?                         
003100     03 RESP-KVRADER-MAX     PIC 9(5).                                    
003200*                                 MAX INDEX KOPPLAT TILL OCCURS N         
003300*                                 EDAN.                                   
003400     03 RESP-RADER           OCCURS 1 TO 500 TIMES                        
003500                             DEPENDING ON RESP-KVRADER-MAX.               
003600*                                                                         
003700        05 RESP-KDCMD-RAD    PIC X.                                       
003800*                                 RAD-UPPDATERINGSKOMMANDO                
003900*                                  BLANK  = INGENTING                     
004000*                                  D , B  = DELETE                        
004100*                                  R , Ä  = REPLACE                       
004200*                                  I,N,A  = INSERT                        
004300*                                  S , V  = SELECT                        
004400*                                  P , P  = PRINT                         
004500*                                  C , K  = COPY                          
004600        05 RESP-IDDISTR      PIC 9(4).                                    
004700*                                 DISTRIKTNUMMER                          
004800        05 RESP-IDKUNDNR     PIC 9(6).                                    
004900*                                 KUNDNUMMER                              
005000        05 RESP-IDORDNR7     PIC 9(7).                                    
005100*                                 ORDERNUMMER                             
005200        05 RESP-IDKOLLI      PIC 9(5).                                    
005300*                                 KOLLINUMMER                             
005400        05 RESP-TIRFS        PIC 9(10).                                   
005500*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
005600        05 RESP-KDKOLLI      PIC X(8).                                    
005700*                                 KOLLIKOD                                
005800        05 RESP-VKORDBTO     PIC 9(7)V9(1).                               
005900*                                 ORDERVIKT BRUTTO (KG)                   
006000        05 RESP-VLORDBTO     PIC 9(4)V9(3).                               
006100*                                 ORDERVOLYM BRUTTO (M3)                  
006200        05 RESP-IDPSN        PIC X(4).                                    
006300*                                 PROPER SHIPPING NAME                    
006400        05 RESP-IDPRODNR     PIC 9(7).                                    
006500*                                 PRODUKTIONSNUMMER                       
006600        05 RESP-FLCROSS      PIC X.                                       
006700*                                 CROSS-DOCK KOLLI FLAGGA                 
006800        05 RESP-IDMSG-ERROR-LINE                                          
006900                             PIC X(3).                                    
007000*                                 FELMEDDELANDE ID                        
007100*** END OF VILMAII-COPY LENGTH= 35568 BYTES                               
