000100 01  RESP-WL0187O1.                                                       
000200*                                 RESPONS TO PGM WL0187                   
000300     03 RESP-IDDC-KEY        PIC X(2).                                    
000400*                                 IDENTIFIERARE LAGER                     
000500     03 RESP-IDTRPTNR-KEY    PIC 9(3).                                    
000600*                                 TRANSPORTIDENTITET                      
000700     03 RESP-IDLBBET-KEY     PIC X(12).                                   
000800*                                 LASTBÄRARBETECKNING                     
000900     03 RESP-KDFARLIG-KEY    PIC X.                                       
001000*                                 ALLMÄN FLAGGA                           
001100     03 RESP-SUMMOR.                                                      
001200*                                                                         
001300        05 RESP-KVKOLLI-TOT  PIC 9(4).                                    
001400*                                 ANTAL KOLLI                             
001500        05 RESP-VKORDBTO-TOT PIC 9(6)V9(1).                               
001600*                                 ORDERVIKT BRUTTO (KG)                   
001700        05 RESP-VLORDBTO-TOT PIC 9(4)V9(3).                               
001800*                                 ORDERVOLYM BRUTTO (M3)                  
001900     03 RESP-FLAVSLUTA       PIC X.                                       
002000*                                 ALLMÄN FLAGGA                           
002100     03 RESP-IDDISTR-DOLD    PIC 9(4).                                    
002200*                                 DISTRIKTNUMMER                          
002300     03 RESP-KDTRTYP         PIC X.                                       
002400*                                 IMS TRANSAKTIONSTYP                     
002500     03 RESP-KVRADER-MAX     PIC 9(5).                                    
002600*                                 MAX INDEX KOPPLAT TILL OCCURS N         
002700*                                 EDAN.                                   
002800     03 RESP-RADER           OCCURS 1 TO 500 TIMES                        
002900                             DEPENDING ON RESP-KVRADER-MAX.               
003000*                                                                         
003100        05 RESP-KDCMD-RAD    PIC X.                                       
003200*                                 RAD-UPPDATERINGSKOMMANDO                
003300*                                  BLANK  = INGENTING                     
003400*                                  D , B  = DELETE                        
003500*                                  R , Ä  = REPLACE                       
003600*                                  I,N,A  = INSERT                        
003700*                                  S , V  = SELECT                        
003800*                                  P , P  = PRINT                         
003900*                                  C , K  = COPY                          
004000        05 RESP-IDDISTR      PIC 9(4).                                    
004100*                                 DISTRIKTNUMMER                          
004200        05 RESP-IDKUNDNR     PIC 9(6).                                    
004300*                                 KUNDNUMMER                              
004400        05 RESP-KDFAKTYP     PIC X.                                       
004500*                                 FAKTURATYP                              
004600        05 RESP-IDORDNR7     PIC 9(7).                                    
004700*                                 ORDERNUMMER                             
004800        05 RESP-IDKOLLI      PIC 9(5).                                    
004900*                                 KOLLINUMMER                             
005000        05 RESP-TIRFS        PIC 9(10).                                   
005100*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
005200        05 RESP-KDKOLLI      PIC X(8).                                    
005300*                                 KOLLIKOD                                
005400        05 RESP-VKORDBTO     PIC 9(6)V9(1).                               
005500*                                 ORDERVIKT BRUTTO (KG)                   
005600        05 RESP-VLORDBTO     PIC 9(4)V9(3).                               
005700*                                 ORDERVOLYM BRUTTO (M3)                  
005800        05 RESP-IDPSN        PIC X(4).                                    
005900*                                 PROPER SHIPPING NAME                    
006000        05 RESP-IDPRODNR     PIC 9(7).                                    
006100*                                 PRODUKTIONSNUMMER                       
006200        05 RESP-FLCROSS      PIC X.                                       
006300*                                 CROSS-DOCK KOLLI FLAGGA                 
006400        05 RESP-IDMSG-ERROR-LINE                                          
006500                             PIC X(3).                                    
006600*                                 FELMEDDELANDE ID                        
006700*** END OF VILMAII-COPY LENGTH= 35547 BYTES                               
