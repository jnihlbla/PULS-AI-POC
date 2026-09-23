000100 01  RAPP-WDM601.                                                         
000200*                                 BYTESREGISTER                           
000300*                                 LAGRING AV RETURER                      
000400*                                 RAPPORTHUVUD SEGMENT                    
000500*                                 FYSISK NYCKEL: WDM601KY                 
000600*                                 (IDDISTR + IDBYTRAP)                    
000700     03 RAPP-IDDISTR         PIC S9(5)           COMP-3.                  
000800*                                 DISTRIKTNUMMER                          
000900*                                 DISTRICT NUMBER                         
001000     03 RAPP-IDBYTRAP        PIC S9(7)           COMP-3.                  
001100*                                 RAPPORTNUMMER  BYTES                    
001200*                                 REPORTNUMBER   EXCHANGE                 
001300     03 RAPP-ADBYTANK        PIC X(10).                                   
001400*                                 ANKOMSTADRESS BYTESOBJEKT               
001500*                                 ARRIVALADDRESS CORES                    
001600     03 RAPP-FLBYTGAR        PIC X.                                       
001700*                                 GARANTI RAPPORT FLAGGA                  
001800*                                 Y = GARANTI                             
001900*                                 N = EJ GARANTI                          
002000*                                 WARRANTY FLAG                           
002100     03 RAPP-FLBYGODK        PIC X.                                       
002200     03 RAPP-IDFAKT          PIC S9(7)           COMP-3.                  
002300*                                 FAKTURANUMMER                           
002400*                                 INVOICE NO.                             
002500     03 RAPP-IDKUNDNR        PIC S9(7)           COMP-3.                  
002600*                                 KUNDNUMMER                              
002700*                                 CUSTOMER NO                             
002800     03 RAPP-IDDC            PIC X(2).                                    
002900*                                 IDENTIFIERARE LAGER                     
003000*                                 WAREHOUSE IDENTIFIER                    
003100     03 RAPP-IDUSER          PIC X(8).                                    
003200*                                 ANVÄNDARENS SÄKERHETS ID                
003300*                                 USER SECURITY-IDENTITY                  
003400     03 RAPP-KDBYTBEK        PIC X.                                       
003500*                                 MOTTAGNINGSBEKRÄFTELSE                  
003600*                                 M = MOTTAGEN                            
003700*                                 K = KLAR                                
003800*                                   = FÖRSTA GÅNGEN ÄR DEN BLANK          
003900*                                 FINISHED FLAG                           
004000     03 RAPP-KDBYTSTA-AVL    PIC X.                                       
004100*                                 AVLÄST STATUSKOD BYTESOBJEKT            
004200*                                 READ STATUS CODE EXCHANGE CORES         
004300     03 RAPP-KDBYTSTA-RAPP   PIC X.                                       
004400*                                 STATUSKOD BYTESOBJEKT                   
004500*                                 STATUSCODE EXCH CORES                   
004600     03 RAPP-KVRETUR-TOT     PIC S9(7)           COMP-3.                  
004700*                                 ANTAL I RETUR                           
004800*                                 QUANTITY IN RETURN                      
004900     03 RAPP-DAANKDAG        PIC 9(8).                                    
005000*                                 ANKOMSTDAG                              
005100*                                 RECEIVING DATE                          
005200     03 RAPP-DAREGDAT        PIC 9(8).                                    
005300*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
005400*                                 REGISTRATION DATE (YYYYMMDD)            
005500     03 RAPP-DAREGDAT-GODK   PIC 9(8).                                    
005600*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
005700*                                 REGISTRATION DATE (YYYYMMDD)            
005800*** END OF VILMAII-COPY LENGTH= 68 BYTES                                  
