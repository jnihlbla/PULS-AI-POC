000100 01  602CROS-W602CROS.                                                    
000200*                                 REQUEST-COPYTEXT PGM W602CROS           
000300*                                 DC CROSS                                
000400     03 602CROS-IDDC-KEY     PIC X(2).                                    
000500*                                 IDENTIFIERARE LAGER                     
000600*                                 WAREHOUSE IDENTIFIER                    
000700     03 602CROS-TIRFSDAT-KEY PIC 9(6).                                    
000800*                                 KLART FÖR TRANSPORT ÅÅMMDD              
000900*                                 READY FOR SHIPMENT  YYMMDD              
001000     03 602CROS-IDDISTR-KEY  PIC 9(4).                                    
001100*                                 DISTRIKTNUMMER                          
001200*                                 DISTRICT NUMBER                         
001300     03 602CROS-IDKUNDNR-KEY PIC 9(6).                                    
001400*                                 KUNDNUMMER                              
001500*                                 CUSTOMER NO                             
001600     03 602CROS-IDTRPTNR-CROSS-KEY                                        
001700                             PIC 9(3).                                    
001800*                                 TRANSPORTIDENTITET DCCROSS              
001900*                                 TRANSPORT IDENTITY DCCROSS              
002000     03 602CROS-IDLEVNR-KEY  PIC X(5).                                    
002100*                                 LEVERANTÖRNUMMER                        
002200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
002300     03 602CROS-IDSUPREF-KEY PIC X(10).                                   
002400*                                 LEVERANTöRSREF.                         
002500*                                 SUPPLIER REF.                           
002600     03 602CROS-NOTREC-KEY   PIC X.                                       
002700*                                 ALLMÄN FLAGGA                           
002800*                                 GENERAL FLAG                            
002900     03 602CROS-KVRADER      PIC 9(5).                                    
003000*                                 ANTAL RADER                             
003100*                                 NUMBER OF LINES                         
003200     03 602CROS-UNREC-KVKOLLI                                             
003300                             PIC 9(4).                                    
003400*                                 ANTAL KOLLI                             
003500*                                 NBR OF CASES                            
003600     03 602CROS-UNREC-VKORDBTO                                            
003700                             PIC 9(6)V9(1).                               
003800*                                 ORDERVIKT BRUTTO (KG)                   
003900*                                 GROSS WEIGHT (KG)                       
004000     03 602CROS-UNREC-VLORDBTO                                            
004100                             PIC 9(4)V9(3).                               
004200*                                 ORDERVOLYM BRUTTO (M3)                  
004300*                                 GROSS VOLUME PER ORDER (M3)             
004400     03 602CROS-REC-KVKOLLI  PIC 9(4).                                    
004500*                                 ANTAL KOLLI                             
004600*                                 NBR OF CASES                            
004700     03 602CROS-REC-VKORDBTO PIC 9(6)V9(1).                               
004800*                                 ORDERVIKT BRUTTO (KG)                   
004900*                                 GROSS WEIGHT (KG)                       
005000     03 602CROS-REC-VLORDBTO PIC 9(4)V9(3).                               
005100*                                 ORDERVOLYM BRUTTO (M3)                  
005200*                                 GROSS VOLUME PER ORDER (M3)             
005300     03 602CROS-KDCALL       PIC 9(3).                                    
005400*                                 ANROPSTYP                               
005500*                                 CALL TYPE                               
005600     03 602CROS-KDSVAR       PIC X.                                       
005700*                                 SVAR FRÅN PROGRAM ELLER SKÄRM           
005800*                                 RETURN CODE FROM PROGRAM                
005900     03 602CROS-IDMSG-INFO   PIC X(3).                                    
006000*                                 INFORMATIONSMEDDELANDE ID               
006100*                                 INFORMATION MESSAGE ID                  
006200     03 602CROS-KDPGMACT     PIC X.                                       
006300*                                 TYP AV PROGRAMBEARBETNING               
006400*                                 TYPE OF PROGRAM ACTION                  
006500     03 602CROS-NEXT-TRPTNR-CROSS                                         
006600                             PIC 9(3).                                    
006700*                                 TRANSPORTIDENTITET                      
006800*                                 TRANSPORT IDENTITY                      
006900     03 602CROS-TABELLRAD    OCCURS 500 TIMES.                            
007000*                                 GRUPP MED TABELLRADER                   
007100        05 602CROS-KDCMDVAL  PIC X(3).                                    
007200*                                 GENERELL KOMMANDOKOD                    
007300*                                 GENERAL COMMAND-CODE                    
007400        05 602CROS-IDDC-SEND PIC X(2).                                    
007500*                                 SÄNDANDE LAGER                          
007600*                                 SENDING WAREHOUSE                       
007700        05 602CROS-TIRECXDAT PIC 9(6).                                    
007800*                                 DATUM FÖR MOTTAG KLI I KROSS-DC         
007900*                                 DATE CASE BINNING IN CROSS-DC           
008000        05 602CROS-TIRECXTID PIC 9(4).                                    
008100*                                 TID FÖR MOTTAG KOLLI I KROSS-DC         
008200*                                 TIME CASE BINNING IN CROSS-DC           
008300        05 602CROS-IDLBBET   PIC X(12).                                   
008400*                                 LASTBÄRARBETECKNING                     
008500*                                 TRAILER NUMBER                          
008600        05 602CROS-IDDISTR   PIC 9(4).                                    
008700*                                 DISTRIKTNUMMER                          
008800*                                 DISTRICT NUMBER                         
008900        05 602CROS-IDKUNDNR  PIC 9(6).                                    
009000*                                 KUNDNUMMER                              
009100*                                 CUSTOMER NO                             
009200        05 602CROS-IDORDNR5  PIC 9(5).                                    
009300*                                 ORDERNUMMER                             
009400*                                 ORDER NUMBER                            
009500        05 602CROS-IDKOLLI   PIC 9(5).                                    
009600*                                 KOLLINUMMER                             
009700*                                 CASE NUMBER                             
009800        05 602CROS-TIRFSDAT  PIC 9(6).                                    
009900*                                 KLART FÖR TRANSPORT ÅÅMMDD              
010000*                                 READY FOR SHIPMENT  YYMMDD              
010100        05 602CROS-TISKEPPN  PIC 9(6).                                    
010200*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
010300*                                 SHIPPING DATE    (YYMMDD)               
010400        05 602CROS-KDKOLLI   PIC X(8).                                    
010500*                                 KOLLIKOD                                
010600*                                 KOLLI CODE                              
010700        05 602CROS-VKORDBTO  PIC Z(5)9.9.                                 
010800*                                 ORDERVIKT BRUTTO (KG)                   
010900*                                 GROSS WEIGHT (KG)                       
011000        05 602CROS-VLORDBTO  PIC Z(3)9.9(3).                              
011100*                                 ORDERVOLYM BRUTTO (M3)                  
011200*                                 GROSS VOLUME PER ORDER (M3)             
011300        05 602CROS-IDPSN     PIC 9(3).                                    
011400*                                 PROPER SHIPPING NAME                    
011500*                                 PROPER SHIPPING NAME                    
011600        05 602CROS-IDPRODNR  PIC 9(7).                                    
011700*                                 PRODUKTIONSNUMMER                       
011800*                                 PRODUCTION NUMBER                       
011900        05 602CROS-IDTRPTNR-CROSS                                         
012000                             PIC 9(3).                                    
012100*                                 TRANSPORTIDENTITET DCCROSS              
012200*                                 TRANSPORT IDENTITY DCCROSS              
012300        05 602CROS-IDLEVNR   PIC X(5).                                    
012400*                                 LEVERANTÖRNUMMER                        
012500*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
012600        05 602CROS-IDSUPREF  PIC X(10).                                   
012700*                                 LEVERANTöRSREF.                         
012800*                                 SUPPLIER REF.                           
012900        05 602CROS-IDMSG-ERROR-LINE                                       
013000                             PIC X(3).                                    
013100*                                 FELMEDDELANDE ID                        
013200*                                 ERROR MESSAGE ID                        
013300*** END OF VILMAII-COPY LENGTH= 57089 BYTES                               
