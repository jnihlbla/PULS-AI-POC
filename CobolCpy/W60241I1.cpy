000100 01  REQU-W60241I1.                                                       
000200*                                 REQUEST-COPYTEXT PGM W60241             
000300*                                 DC CROSS                                
000400     03 REQU-IDDC-KEY        PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 REQU-TIRFSDAT-KEY    PIC 9(6).                                    
000800*                                 KLART FÖR TRANSPORT ÅÅMMDD              
000900*                                 READY FOR SHIPMENT  YYMMDD              
001000     03 REQU-IDDISTR-KEY     PIC 9(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200*                                 DISTRICT NUMBER                         
001300     03 REQU-IDKUNDNR-KEY    PIC 9(6).                                    
001400*                                 KUNDNUMMER                              
001500*                                 CUSTOMER NO                             
001600     03 REQU-IDTRPTNR-CROSS-KEY                                           
001700                             PIC 9(3).                                    
001800*                                 TRANSPORTIDENTITET DCCROSS              
001900*                                 TRANSPORT IDENTITY DCCROSS              
002000     03 REQU-IDLEVNR-KEY     PIC X(5).                                    
002100*                                 LEVERANTÖRNUMMER                        
002200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002300     03 REQU-IDSUPREF-KEY    PIC X(10).                                   
002400*                                 LEVERANTöRSREF.                         
002500*                                 SUPPLIER REF.                           
002600     03 REQU-NOTREC-KEY      PIC X.                                       
002700*                                 ALLMÄN FLAGGA                           
002800*                                 GENERAL FLAG                            
002900     03 REQU-TOT-UNREC-KVKOLLI                                            
003000                             PIC 9(4).                                    
003100*                                 ANTAL KOLLI                             
003200*                                 NBR OF CASES                            
003300     03 REQU-TOT-UNREC-VKORDBTO                                           
003400                             PIC Z(5)9.9.                                 
003500*                                 ORDERVIKT BRUTTO (KG)                   
003600*                                 GROSS WEIGHT (KG)                       
003700     03 REQU-TOT-UNREC-VLORDBTO                                           
003800                             PIC Z(3)9.9(3).                              
003900*                                 ORDERVOLYM BRUTTO (M3)                  
004000*                                 GROSS VOLUME PER ORDER (M3)             
004100     03 REQU-TOT-REC-KVKOLLI PIC 9(4).                                    
004200*                                 ANTAL KOLLI                             
004300*                                 NBR OF CASES                            
004400     03 REQU-TOT-REC-VKORDBTO                                             
004500                             PIC Z(5)9.9.                                 
004600*                                 ORDERVIKT BRUTTO (KG)                   
004700*                                 GROSS WEIGHT (KG)                       
004800     03 REQU-TOT-REC-VLORDBTO                                             
004900                             PIC Z(3)9.9(3).                              
005000*                                 ORDERVOLYM BRUTTO (M3)                  
005100*                                 GROSS VOLUME PER ORDER (M3)             
005200     03 REQU-KVRADER         PIC 9(5).                                    
005300*                                 ANTAL RADER                             
005400*                                 NUMBER OF LINES                         
005500     03 REQU-TABELLRAD       OCCURS 500 TIMES.                            
005600*                                 GRUPP MED TABELLRADER                   
005700        05 REQU-KDCMDVAL     PIC X(3).                                    
005800*                                 GENERELL KOMMANDOKOD                    
005900*                                 GENERAL COMMAND-CODE                    
006000        05 REQU-IDDC-SEND    PIC X(2).                                    
006100*                                 SÄNDANDE LAGER                          
006200*                                 SENDING WAREHOUSE                       
006300        05 REQU-TIRECXDAT    PIC 9(6).                                    
006400*                                 DATUM FÖR MOTTAG KLI I KROSS-DC         
006500*                                 DATE CASE BINNING IN CROSS-DC           
006600        05 REQU-TIRECXTID    PIC 9(4).                                    
006700*                                 TID FÖR MOTTAG KOLLI I KROSS-DC         
006800*                                 TIME CASE BINNING IN CROSS-DC           
006900        05 REQU-IDLBBET      PIC X(12).                                   
007000*                                 LASTBÄRARBETECKNING                     
007100*                                 TRAILER NUMBER                          
007200        05 REQU-IDDISTR      PIC 9(4).                                    
007300*                                 DISTRIKTNUMMER                          
007400*                                 DISTRICT NUMBER                         
007500        05 REQU-IDKUNDNR     PIC 9(6).                                    
007600*                                 KUNDNUMMER                              
007700*                                 CUSTOMER NO                             
007800        05 REQU-IDORDNR5     PIC 9(5).                                    
007900*                                 ORDERNUMMER                             
008000*                                 ORDER NUMBER                            
008100        05 REQU-IDKOLLI      PIC 9(5).                                    
008200*                                 KOLLINUMMER                             
008300*                                 CASE NUMBER                             
008400        05 REQU-TIRFSDAT     PIC 9(6).                                    
008500*                                 KLART FÖR TRANSPORT ÅÅMMDD              
008600*                                 READY FOR SHIPMENT  YYMMDD              
008700        05 REQU-TISKEPPN     PIC 9(6).                                    
008800*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
008900*                                 SHIPPING DATE    (YYMMDD)               
009000        05 REQU-KDKOLLI      PIC X(8).                                    
009100*                                 KOLLIKOD                                
009200*                                 KOLLI CODE                              
009300        05 REQU-VKORDBTO     PIC Z(5)9.9.                                 
009400*                                 ORDERVIKT BRUTTO (KG)                   
009500*                                 GROSS WEIGHT (KG)                       
009600        05 REQU-VLORDBTO     PIC Z(3)9.9(3).                              
009700*                                 ORDERVOLYM BRUTTO (M3)                  
009800*                                 GROSS VOLUME PER ORDER (M3)             
009900        05 REQU-IDPSN        PIC 9(3).                                    
010000*                                 PROPER SHIPPING NAME                    
010100*                                 PROPER SHIPPING NAME                    
010200        05 REQU-IDPRODNR     PIC 9(7).                                    
010300*                                 PRODUKTIONSNUMMER                       
010400*                                 PRODUCTION NUMBER                       
010500        05 REQU-IDTRPTNR-CROSS                                            
010600                             PIC 9(3).                                    
010700*                                 TRANSPORTIDENTITET DCCROSS              
010800*                                 TRANSPORT IDENTITY DCCROSS              
010900        05 REQU-IDLEVNR      PIC X(5).                                    
011000*                                 LEVERANTÖRNUMMER                        
011100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
011200        05 REQU-IDSUPREF     PIC X(10).                                   
011300*                                 LEVERANTöRSREF.                         
011400*                                 SUPPLIER REF.                           
011500        05 REQU-IDMSG-ERROR-LINE                                          
011600                             PIC X(3).                                    
011700*                                 FELMEDDELANDE ID                        
011800*                                 ERROR MESSAGE ID                        
011900*** END OF VILMAII-COPY LENGTH= 57082 BYTES                               
