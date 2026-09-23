000100 01  RESP-W60115O1.                                                       
000200*                                 RESPCOPYTEXT TILL W60115.               
000300*                                                                         
000400     03 RESP-IDLEVNR-START   PIC X(5).                                    
000500*                                 LEVERANTÖRNUMMER                        
000600*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
000700     03 RESP-IDLEVNR-NEXT    PIC X(5).                                    
000800*                                 LEVERANTÖRNUMMER                        
000900*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001000     03 RESP-IDFS-START      PIC X(8).                                    
001100*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001200*                                 ADVICE NOTE NUMBER ODETTE               
001300     03 RESP-IDFS-NEXT       PIC X(8).                                    
001400*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001500*                                 ADVICE NOTE NUMBER ODETTE               
001600     03 RESP-TIAVIDAT-START  PIC 9(6).                                    
001700*                                 AVISERINGSDATUM (YYMMDD)                
001800*                                 ADVICE NOTE DATE                        
001900     03 RESP-TIAVIDAT-NEXT   PIC 9(6).                                    
002000*                                 AVISERINGSDATUM (YYMMDD)                
002100*                                 ADVICE NOTE DATE                        
002200     03 RESP-IDLBBET-START   PIC X(12).                                   
002300*                                 LASTBÄRARBETECKNING                     
002400*                                 TRAILER NUMBER                          
002500     03 RESP-IDLBBET-NEXT    PIC X(12).                                   
002600*                                 LASTBÄRARBETECKNING                     
002700*                                 TRAILER NUMBER                          
002800     03 RESP-OMSTART-INDX    PIC X(2).                                    
002900     03 RESP-FILLER          OCCURS 5 TIMES.                              
003000*                                 UPDATE                                  
003100        05 RESP-ADINLOMR-LPL PIC X(4).                                    
003200*                                 LOSSNINGSPLATS                          
003300*                                 UNLOADING AREA                          
003400        05 RESP-SUPROC-LPL   PIC 9(3).                                    
003500     03 RESP-IDLBBET-UPD     PIC X(12).                                   
003600*                                 LASTBÄRARBETECKNING                     
003700*                                 TRAILER NUMBER                          
003800     03 RESP-IDLBBET-UPD-ATTR                                             
003900                             PIC X(2).                                    
004000*                                 MFS ATTRIBUTFÄLT                        
004100     03 RESP-FLKLAR-UPD      PIC X.                                       
004200*                                 AVSLUTNINGSMARKERING                    
004300*                                 FINISHED FLAG                           
004400     03 RESP-FLKLAR-UPD-ATTR PIC X(2).                                    
004500*                                 MFS ATTRIBUTFÄLT                        
004600     03 RESP-ADINLOMR-LPL-UPD                                             
004700                             PIC X(4).                                    
004800*                                 LOSSNINGSPLATS                          
004900*                                 UNLOADING AREA                          
005000     03 RESP-ADINLOMR-LPL-UPD-ATTR                                        
005100                             PIC X(2).                                    
005200*                                 MFS ATTRIBUTFÄLT                        
005300     03 RESP-TEMFSINF        PIC X(55).                                   
005400*                                 INFORMATIONSMEDDELANDE                  
005500*                                 INFORMATION MESSAGE                     
005600     03 RESP-TEMFSFEL        PIC X(40).                                   
005700*                                 MFS FELMEDDELANDE                       
005800*                                 MFS ERROR MESSAGE                       
005900     03 RESP-FLOMSTART       PIC X.                                       
006000*                                 JA/NEJ-FLAGGA                           
006100     03 RESP-KVRADER         PIC 9(5).                                    
006200*                                 ANTAL RADER                             
006300*                                 NUMBER OF LINES                         
006400     03 RESP-UPDATE          OCCURS 500 TIMES.                            
006500*                                 UPDATE                                  
006600        05 RESP-KDCMDVAL-LINE                                             
006700                             PIC X(3).                                    
006800*                                 GENERELL KOMMANDOKOD                    
006900*                                 GENERAL COMMAND-CODE                    
007000        05 RESP-KDCMDVAL-LINE-ATTR                                        
007100                             PIC X(2).                                    
007200*                                 MFS ATTRIBUTFÄLT                        
007300        05 RESP-IDLEVNR-LINE PIC X(5).                                    
007400*                                 LEVERANTÖRNUMMER                        
007500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
007600        05 RESP-IDLEVNR-LINE-ATTR                                         
007700                             PIC X(2).                                    
007800*                                 MFS ATTRIBUTFÄLT                        
007900        05 RESP-IDFS-LINE    PIC X(8).                                    
008000*                                 FÖLJESEDELSNUMMER ENL ODETTE            
008100*                                 ADVICE NOTE NUMBER ODETTE               
008200        05 RESP-IDFS-LINE-ATTR                                            
008300                             PIC X(2).                                    
008400*                                 MFS ATTRIBUTFÄLT                        
008500        05 RESP-FLFEL-LINE   PIC X.                                       
008600*                                 ALLMÄN FELFLAGGA                        
008700*                                 GENERAL ERROR FLAG                      
008800        05 RESP-FLFEL-LINE-ATTR                                           
008900                             PIC X(2).                                    
009000*                                 MFS ATTRIBUTFÄLT                        
009100        05 RESP-TIAVIDAT-LINE                                             
009200                             PIC X(6).                                    
009300*                                 AVISERINGSDATUM (YYMMDD)                
009400*                                 ADVICE NOTE DATE                        
009500        05 RESP-IDLBBET-LINE PIC X(12).                                   
009600*                                 LASTBÄRARBETECKNING                     
009700*                                 TRAILER NUMBER                          
009800        05 RESP-KVPARTI-LINE PIC X(7).                                    
009900        05 RESP-ADINLOMR-FB-LINE                                          
010000                             PIC X(4).                                    
010100*                                 INLEVERANSOMRÅDE                        
010200*                                 RECEIVING AREA                          
010300        05 RESP-IDSHIPM-LINE PIC X(7).                                    
010400*                                 SKEPPNINGSNUMMER                        
010500*                                 SHIPMENT NO                             
010600*** END OF VILMAII-COPY LENGTH= 30723 BYTES                               
