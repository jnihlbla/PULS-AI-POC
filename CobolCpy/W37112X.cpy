000100 01  W37112X.                                                             
000200*                                 BYTESREGISTER                           
000300*                                 LAGRING AV RETURER                      
000400*                                 RAPPORTHUVUD SEGMENT                    
000500*                                 OBJEKT SEGMENT TO DATALAKE              
000600     03 RAPP-IDDISTR         PIC Z(3)9.                                   
000700*                                 DISTRIKTNUMMER                          
000800*                                 DISTRICT NUMBER                         
000900     03 RAPP-IDBYTRAP        PIC 9(7).                                    
001000*                                 RAPPORTNUMMER  BYTES                    
001100*                                 REPORTNUMBER   EXCHANGE                 
001200     03 RAPP-ADBYTANK        PIC X(10).                                   
001300*                                 ANKOMSTADRESS BYTESOBJEKT               
001400*                                 ARRIVALADDRESS CORES                    
001500     03 RAPP-FLBYTGAR        PIC X.                                       
001600*                                 GARANTI RAPPORT FLAGGA                  
001700*                                 Y = GARANTI                             
001800*                                 N = EJ GARANTI                          
001900*                                 WARRANTY FLAG                           
002000     03 RAPP-FLBYGODK        PIC X.                                       
002100     03 RAPP-IDFAKT          PIC Z(6)9.                                   
002200*                                 FAKTURANUMMER                           
002300*                                 INVOICE NO.                             
002400     03 RAPP-IDKUNDNR        PIC Z(5)9.                                   
002500*                                 KUNDNUMMER                              
002600*                                 CUSTOMER NO                             
002700     03 RAPP-IDDC            PIC X(2).                                    
002800*                                 IDENTIFIERARE LAGER                     
002900*                                 WAREHOUSE IDENTIFIER                    
003000     03 RAPP-IDUSER          PIC X(8).                                    
003100*                                 ANVÄNDARENS SÄKERHETS ID                
003200*                                 USER SECURITY-IDENTITY                  
003300     03 RAPP-KDBYTBEK        PIC X.                                       
003400*                                 MOTTAGNINGSBEKRÄFTELSE                  
003500*                                 M = MOTTAGEN                            
003600*                                 K = KLAR                                
003700*                                   = FÖRSTA GÅNGEN ÄR DEN BLANK          
003800*                                 FINISHED FLAG                           
003900     03 RAPP-KDBYTSTA-AVL    PIC X.                                       
004000*                                 AVLÄST STATUSKOD BYTESOBJEKT            
004100*                                 READ STATUS CODE EXCHANGE CORES         
004200     03 RAPP-KDBYTSTA-RAPP   PIC X.                                       
004300*                                 STATUSKOD BYTESOBJEKT                   
004400*                                 STATUSCODE EXCH CORES                   
004500     03 RAPP-KVRETUR-TOT     PIC -(6)9.                                   
004600*                                 ANTAL I RETUR                           
004700*                                 QUANTITY IN RETURN                      
004800     03 RAPP-DAANKDAG        PIC 9(8).                                    
004900*                                 ANKOMSTDAG                              
005000*                                 RECEIVING DATE                          
005100     03 RAPP-DAREGDAT        PIC 9(8).                                    
005200*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
005300*                                 REGISTRATION DATE (YYYYMMDD)            
005400     03 RAPP-DAREGDAT-GODK   PIC 9(8).                                    
005500*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
005600*                                 REGISTRATION DATE (YYYYMMDD)            
005700     03 OBJ-IDBYTRAD         PIC Z(4)9.                                   
005800*                                 RADNUMMER                               
005900*                                 LINE NO                                 
006000     03 OBJ-IDARTNR-OBJ      PIC Z(7)9.                                   
006100*                                 OBJEKTNUMMER                            
006200     03 OBJ-IDTABNR          PIC 9(3).                                    
006300*                                 TABELLNUMMER                            
006400*                                 TABELNUMBER                             
006500     03 OBJ-BERADREF         PIC X(10).                                   
006600*                                 KUNDENS RADREFERENS                     
006700*                                 CUSTOMERS ITEM REF.                     
006800     03 OBJ-IDORDER          PIC Z(6)9.                                   
006900*                                 VOLVO PARTS ORDERNUMMER                 
007000*                                 VOLVO PARTS ORDER NUMBER                
007100     03 OBJ-KDBYTREF         PIC X(3).                                    
007200*                                 CENTRAL REFERENS                        
007300*                                 CENTRAL REFERENCE                       
007400     03 OBJ-KDBYTSTA-AVL     PIC X.                                       
007500*                                 AVLÄST STATUSKOD BYTESOBJEKT            
007600*                                 READ STATUS CODE EXCHANGE CORES         
007700     03 OBJ-KDBYTSTA-OBJ     PIC X.                                       
007800*                                 STATUSKOD BYTESOBJEKT                   
007900*                                 STATUSCODE EXCH CORES                   
008000     03 OBJ-KVRETUR-GODK     PIC -(6)9.                                   
008100*                                 ANTAL GODKÄNDA BYTESOBJEKT              
008200*                                 QUANTITY APPROVED OBJECT                
008300     03 OBJ-KVRETUR-URSP     PIC -(6)9.                                   
008400*                                 ANTAL OBJEKT RETURER.                   
008500*                                 QUANTITY OBJECT RETURNS                 
008600     03 OBJ-IDBYTRAP-9KOMPL  PIC 9(7).                                    
008700*                                 RAPPORTNUMMER BYTES 9-KOMPL             
008800*                                 REPORTNUMBER  EXCHANGE 9-COMPL          
008900     03 OBJ-FLSKROT          PIC X.                                       
009000*                                 SKROTNINGSMARKERING                     
009100*                                 SCRAPPING FLAG                          
009200     03 OBJ-FILLERX10        PIC X(10).                                   
009300*** END OF VILMAII-COPY LENGTH= 150 BYTES                                 
