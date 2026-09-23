000100 01  REQU-W60204I1-CTX.                                                   
000200*                                 COPYTEXT FÖR REQU W60204I1              
000300*                                                                         
000400*                                                                         
000500*                                                                         
000600     03 REQU-IDDC-KEY        PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 REQU-IDKR-KEY        PIC X(5).                                    
001000*                                 KONTROLLRAPPORT NUMMER                  
001100*                                 INSPECTION REPORT NUMBER                
001200     03 REQU-IDKOLLINR-KEY   PIC X(5).                                    
001300*                                 KOLLINUMMER                             
001400*                                 CASE NUMBER                             
001500     03 REQU-IDKOLLI-START   PIC X(5).                                    
001600*                                 KOLLINUMMER                             
001700*                                 CASE NUMBER                             
001800     03 REQU-KVRADER         PIC 9(5).                                    
001900*                                 ANTAL RADER                             
002000*                                 NUMBER OF LINES                         
002100     03 REQU-IDSPRAK         PIC X(2).                                    
002200*                                 2-STÄLLIG ISO SPRÅKKOD                  
002300*                                 2-LETTER ISO LANGUAGE CODE              
002400     03 REQU-W60204I1-001-GRP.                                            
002500*                                                                         
002600        05 REQU-KDKOLLI-UPD  PIC X(8).                                    
002700*                                 KOLLIKOD                                
002800*                                 KOLLI CODE                              
002900        05 REQU-IDKOLLI-UPD  PIC 9(5).                                    
003000*                                 KOLLINUMMER                             
003100*                                 CASE NUMBER                             
003200        05 REQU-VKKOLLIB-UPD PIC X(7).                                    
003300*                                 KOLLI-VIKT-BRUTTO                       
003400*                                 GROSS WEIGHT OF PACKAGE                 
003500        05 REQU-DIKOLLIL-UPD PIC 9(4).                                    
003600*                                 KOLLI-LÄNGD                             
003700*                                 CASE LENGTH                             
003800        05 REQU-DIKOLLIB-UPD PIC 9(3).                                    
003900*                                 KOLLI-BREDD                             
004000*                                 CASE WIDTH                              
004100        05 REQU-DIKOLLIH-UPD PIC 9(3).                                    
004200*                                 KOLLI-HÖJD                              
004300*                                 CASE HEIGHT                             
004400        05 REQU-KDCMD-UPD    PIC X.                                       
004500         88 REQU-KDCMD-INGENTING                                          
004600                             VALUE ' '.                                   
004700         88 REQU-KDCMD-DELETE                                             
004800                             VALUE 'D'                                    
004900                             'B'.                                         
005000         88 REQU-KDCMD-REPLACE                                            
005100                             VALUE 'R'                                    
005200                             'Ä'.                                         
005300         88 REQU-KDCMD-INSERT                                             
005400                             VALUE 'I'                                    
005500                             'N'                                          
005600                             'A'.                                         
005700         88 REQU-KDCMD-SELECT                                             
005800                             VALUE 'S'                                    
005900                             'V'.                                         
006000         88 REQU-KDCMD-PRINT VALUE 'P'                                    
006100                             'P'.                                         
006200         88 REQU-KDCMD-COPY  VALUE 'C'                                    
006300                             'K'.                                         
006400*                                 RAD-UPPDATERINGSKOMMANDO                
006500*                                  BLANK  = INGENTING                     
006600*                                  D , B  = DELETE                        
006700*                                  R , Ä  = REPLACE                       
006800*                                  I,N,A  = INSERT                        
006900*                                  S , V  = SELECT                        
007000*                                  P , P  = PRINT                         
007100*                                  C , K  = COPY                          
007200*                                 LINE UPDATE COMMAND                     
007300        05 REQU-KDPERSON-UPD PIC 9(3).                                    
007400*                                 PERSONKOD                               
007500*                                 STAFF CODE                              
007600        05 REQU-BEKRPACK-UPD PIC X(25).                                   
007700*                                 ANSVARIG FÖR PACKNING                   
007800*                                                                         
007900*                                 RESPONSIBLE FOR PACKING                 
008000*                                                                         
008100        05 REQU-KVKRPACK-UPD PIC X(4).                                    
008200*                                 PACKNINGSTID                            
008300*                                 TIME FOR PACKING                        
008400*** END OF VILMAII-COPY LENGTH= 87 BYTES                                  
