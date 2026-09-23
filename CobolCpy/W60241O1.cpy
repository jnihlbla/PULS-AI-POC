000100 01  RESP-W60241O1.                                                       
000200*                                 RESPONS-COPYTEXT FÖR PGM W60241         
000300*                                 DC CROSS                                
000400     03 RESP-UNREC-KVKOLLI   PIC Z(3)9.                                   
000500*                                 ANTAL KOLLI                             
000600*                                 NBR OF CASES                            
000700     03 RESP-UNREC-VKORDBTO  PIC Z(7)9.9.                                 
000800*                                 ORDERVIKT BRUTTO (KG)                   
000900*                                 GROSS WEIGHT (KG)                       
001000     03 RESP-UNREC-VLORDBTO  PIC Z(4)9.9(3).                              
001100*                                 ORDERVOLYM BRUTTO (M3)                  
001200*                                 GROSS VOLUME PER ORDER (M3)             
001300     03 RESP-REC-KVKOLLI     PIC Z(3)9.                                   
001400*                                 ANTAL KOLLI                             
001500*                                 NBR OF CASES                            
001600     03 RESP-REC-VKORDBTO    PIC Z(7)9.9.                                 
001700*                                 ORDERVIKT BRUTTO (KG)                   
001800*                                 GROSS WEIGHT (KG)                       
001900     03 RESP-REC-VLORDBTO    PIC Z(4)9.9(3).                              
002000*                                 ORDERVOLYM BRUTTO (M3)                  
002100*                                 GROSS VOLUME PER ORDER (M3)             
002200     03 RESP-KVRADER         PIC 9(5).                                    
002300*                                 ANTAL RADER                             
002400*                                 NUMBER OF LINES                         
002500     03 RESP-TABELLRAD       OCCURS 500 TIMES.                            
002600*                                 GRUPP MED TABELLRADER                   
002700        05 RESP-KDCMDVAL     PIC X(3).                                    
002800*                                 GENERELL KOMMANDOKOD                    
002900*                                 GENERAL COMMAND-CODE                    
003000        05 RESP-IDDC-SEND    PIC X(2).                                    
003100*                                 SÄNDANDE LAGER                          
003200*                                 SENDING WAREHOUSE                       
003300        05 RESP-TIRECXDAT    PIC Z(6).                                    
003400*                                 DATUM FÖR MOTTAG KLI I KROSS-DC         
003500*                                 DATE CASE BINNING IN CROSS-DC           
003600        05 RESP-TIRECXTID    PIC Z(4).                                    
003700*                                 TID FÖR MOTTAG KOLLI I KROSS-DC         
003800*                                 TIME CASE BINNING IN CROSS-DC           
003900        05 RESP-IDLBBET      PIC X(12).                                   
004000*                                 LASTBÄRARBETECKNING                     
004100*                                 TRAILER NUMBER                          
004200        05 RESP-IDDISTR      PIC Z(3)9.                                   
004300*                                 DISTRIKTNUMMER                          
004400*                                 DISTRICT NUMBER                         
004500        05 RESP-IDKUNDNR     PIC Z(5)9.                                   
004600*                                 KUNDNUMMER                              
004700*                                 CUSTOMER NO                             
004800        05 RESP-IDORDNR5     PIC Z(5).                                    
004900*                                 ORDERNUMMER                             
005000*                                 ORDER NUMBER                            
005100        05 RESP-IDKOLLI      PIC Z(5).                                    
005200*                                 KOLLINUMMER                             
005300*                                 CASE NUMBER                             
005400        05 RESP-TIRFSDAT     PIC 9(6).                                    
005500*                                 KLART FÖR TRANSPORT ÅÅMMDD              
005600*                                 READY FOR SHIPMENT  YYMMDD              
005700        05 RESP-TISKEPPN     PIC 9(6).                                    
005800*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
005900*                                 SHIPPING DATE    (YYMMDD)               
006000        05 RESP-KDKOLLI      PIC X(8).                                    
006100*                                 KOLLIKOD                                
006200*                                 KOLLI CODE                              
006300        05 RESP-VKORDBTO     PIC Z(5)9.9.                                 
006400*                                 ORDERVIKT BRUTTO (KG)                   
006500*                                 GROSS WEIGHT (KG)                       
006600        05 RESP-VLORDBTO     PIC Z(3)9.9(3).                              
006700*                                 ORDERVOLYM BRUTTO (M3)                  
006800*                                 GROSS VOLUME PER ORDER (M3)             
006900        05 RESP-IDPSN        PIC 9(3).                                    
007000*                                 PROPER SHIPPING NAME                    
007100*                                 PROPER SHIPPING NAME                    
007200        05 RESP-IDPRODNR     PIC Z(6)9.                                   
007300*                                 PRODUKTIONSNUMMER                       
007400*                                 PRODUCTION NUMBER                       
007500        05 RESP-IDTRPTNR-CROSS                                            
007600                             PIC X(3).                                    
007700*                                 TRANSPORTIDENTITET DCCROSS              
007800*                                 TRANSPORT IDENTITY DCCROSS              
007900        05 RESP-IDLEVNR      PIC X(5).                                    
008000*                                 LEVERANTÖRNUMMER                        
008100*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
008200        05 RESP-IDSUPREF     PIC X(10).                                   
008300*                                 LEVERANTöRSREF.                         
008400*                                 SUPPLIER REF.                           
008500        05 RESP-IDMSG-ERROR-LINE                                          
008600                             PIC X(3).                                    
008700*                                 FELMEDDELANDE ID                        
008800*                                 ERROR MESSAGE ID                        
008900*** END OF VILMAII-COPY LENGTH= 57051 BYTES                               
