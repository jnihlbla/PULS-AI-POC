000100 01  RESP-W60204O1.                                                       
000200*                                 MOD-COPYTEXT                            
000300*                                 FÖR W60204                              
000400     03 RESP-IDKOLLI-START   PIC X(5).                                    
000500*                                 KOLLINUMMER                             
000600*                                 CASE NUMBER                             
000700     03 RESP-IDKOLLI-NEXT    PIC X(5).                                    
000800*                                 KOLLINUMMER                             
000900*                                 CASE NUMBER                             
001000     03 RESP-IDARTNR         PIC Z(8)9.                                   
001100*                                 ARTIKELNUMMER                           
001200*                                 PART NUMBER                             
001300     03 RESP-BEART           PIC X(25).                                   
001400*                                 ARTIKELBENÄMNING                        
001500*                                 PART DESCRIPTION                        
001600     03 RESP-KDKRSTA         PIC X.                                       
001700*                                 KONTROLLRAPPORT STATUS                  
001800*                                 INSPECTION REPORT STATUS                
001900     03 RESP-TIREGDAT        PIC 9(6).                                    
002000*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
002100*                                 REGISTRATION DATE (YYMMDD)              
002200     03 RESP-IDLEVNR         PIC X(5).                                    
002300*                                 LEVERANTÖRNUMMER                        
002400*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002500     03 RESP-IDLEVG          PIC Z(4)9.                                   
002600*                                 LEVERANTÖRS GODSADRESS NUMMER           
002700*                                 SUPPLIER WAREHOUSE NUMBER               
002800     03 RESP-BELEV           PIC X(35).                                   
002900*                                 LEVERANTÖRSNAMN                         
003000*                                 SUPPLIER NAME                           
003100     03 RESP-KVRADER         PIC 9(5).                                    
003200*                                 ANTAL RADER                             
003300*                                 NUMBER OF LINES                         
003400     03 RESP-KDKOLLI-UPD-ATTR                                             
003500                             PIC X(2).                                    
003600*                                 MFS ATTRIBUTFÄLT                        
003700     03 RESP-KDKOLLI-UPD     PIC X(8).                                    
003800*                                 KOLLIKOD                                
003900*                                 KOLLI CODE                              
004000     03 RESP-IDKOLLI-UPD-ATTR                                             
004100                             PIC X(2).                                    
004200*                                 MFS ATTRIBUTFÄLT                        
004300     03 RESP-IDKOLLI-UPD     PIC Z(4)9.                                   
004400*                                 KOLLINUMMER                             
004500*                                 CASE NUMBER                             
004600     03 RESP-VKKOLLIB-UPD-ATTR                                            
004700                             PIC X(2).                                    
004800*                                 MFS ATTRIBUTFÄLT                        
004900     03 RESP-VKKOLLIB-UPD    PIC Z(4)9.9.                                 
005000*                                 KOLLI-VIKT-BRUTTO                       
005100*                                 GROSS WEIGHT OF PACKAGE                 
005200     03 RESP-DIKOLLIL-UPD-ATTR                                            
005300                             PIC X(2).                                    
005400*                                 MFS ATTRIBUTFÄLT                        
005500     03 RESP-DIKOLLIL-UPD    PIC Z(3)9.                                   
005600*                                 KOLLI-LÄNGD                             
005700*                                 CASE LENGTH                             
005800     03 RESP-DIKOLLIB-UPD-ATTR                                            
005900                             PIC X(2).                                    
006000*                                 MFS ATTRIBUTFÄLT                        
006100     03 RESP-DIKOLLIB-UPD    PIC Z(2)9.                                   
006200*                                 KOLLI-BREDD                             
006300*                                 CASE WIDTH                              
006400     03 RESP-DIKOLLIH-UPD-ATTR                                            
006500                             PIC X(2).                                    
006600*                                 MFS ATTRIBUTFÄLT                        
006700     03 RESP-DIKOLLIH-UPD    PIC Z(2)9.                                   
006800*                                 KOLLI-HÖJD                              
006900*                                 CASE HEIGHT                             
007000     03 RESP-KDCMD-UPD-ATTR  PIC X(2).                                    
007100*                                 MFS ATTRIBUTFÄLT                        
007200     03 RESP-KDCMD-UPD       PIC X.                                       
007300*                                 RAD-UPPDATERINGSKOMMANDO                
007400*                                  BLANK  = INGENTING                     
007500*                                  D , B  = DELETE                        
007600*                                  R , Ä  = REPLACE                       
007700*                                  I,N,A  = INSERT                        
007800*                                  S , V  = SELECT                        
007900*                                  P , P  = PRINT                         
008000*                                  C , K  = COPY                          
008100*                                 LINE UPDATE COMMAND                     
008200     03 RESP-KDPERSON-UPD-ATTR                                            
008300                             PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500     03 RESP-KDPERSON-UPD    PIC Z(2)9.                                   
008600*                                 PERSONKOD                               
008700*                                 STAFF CODE                              
008800     03 RESP-BEKRPACK-UPD-ATTR                                            
008900                             PIC X(2).                                    
009000*                                 MFS ATTRIBUTFÄLT                        
009100     03 RESP-BEKRPACK-UPD    PIC X(25).                                   
009200*                                 ANSVARIG FÖR PACKNING                   
009300*                                                                         
009400*                                 RESPONSIBLE FOR PACKING                 
009500*                                                                         
009600     03 RESP-KVKRPACK-UPD-ATTR                                            
009700                             PIC X(2).                                    
009800*                                 MFS ATTRIBUTFÄLT                        
009900     03 RESP-KVKRPACK-UPD    PIC X(4).                                    
010000*                                 PACKNINGSTID                            
010100*                                 TIME FOR PACKING                        
010200     03 RESP-KVKRPACK-UT     PIC Z9.9.                                    
010300*                                 PACKNINGSTID                            
010400*                                 TIME FOR PACKING                        
010500     03 RESP-TIKRPACK        PIC 9(6).                                    
010600*                                 PACKNINGSDATUM                          
010700*                                 DATE OF PACKING                         
010800     03 RESP-LINE            OCCURS 500 TIMES.                            
010900        05 RESP-IDKOLLI-LINE PIC Z(4)9.                                   
011000*                                 KOLLINUMMER                             
011100*                                 CASE NUMBER                             
011200        05 RESP-VKKOLLIB-LINE                                             
011300                             PIC Z(4)9.9.                                 
011400*                                 KOLLI-VIKT-BRUTTO                       
011500*                                 GROSS WEIGHT OF PACKAGE                 
011600        05 RESP-DIKOLLIL-LINE                                             
011700                             PIC Z(3)9.                                   
011800*                                 KOLLI-LÄNGD                             
011900*                                 CASE LENGTH                             
012000        05 RESP-DIKOLLIB-LINE                                             
012100                             PIC Z(2)9.                                   
012200*                                 KOLLI-BREDD                             
012300*                                 CASE WIDTH                              
012400        05 RESP-DIKOLLIH-LINE                                             
012500                             PIC Z(2)9.                                   
012600*                                 KOLLI-HÖJD                              
012700*                                 CASE HEIGHT                             
012800*** END OF VILMAII-COPY LENGTH= 11194 BYTES                               
