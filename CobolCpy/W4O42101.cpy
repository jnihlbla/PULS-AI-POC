000100 01  MOD-W4O42101.                                                        
000200*                                                                         
000300     03 MOD-IDTRANS          PIC X(4).                                    
000400*                                 BILDNUMMER                              
000500     03 MOD-TEMFSFEL         PIC X(40).                                   
000600*                                 MFS FELMEDDELANDE                       
000700     03 MOD-IDDISTR-IN       PIC X(4).                                    
000800*                                 DISTRIKTNUMMER                          
000900     03 MOD-IDDISTR-UT       PIC X(4).                                    
001000*                                 DISTRIKTNUMMER                          
001100     03 MOD-IDKUNDNR-IN      PIC X(6).                                    
001200*                                 KUNDNUMMER                              
001300     03 MOD-IDKUNDNR-UT      PIC X(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 MOD-IDDC-IN          PIC X(2).                                    
001600*                                 IDENTIFIERARE LAGER                     
001700     03 MOD-IDDC-UT          PIC X(2).                                    
001800*                                 IDENTIFIERARE LAGER                     
001900     03 MOD-BEGMT-RAD1       PIC X(35).                                   
002000*                                 GODSMOTTAGARNAMN RAD 1                  
002100     03 MOD-BEGMT-RAD2       PIC X(35).                                   
002200*                                 GODSMOTTAGARNAMN RAD 2                  
002300     03 MOD-ADGMT-GATA       PIC X(35).                                   
002400*                                 GODSMOTTAGARADRESS GATA                 
002500     03 MOD-ADGMT-PADR       PIC X(35).                                   
002600*                                 GODSMOTTAGARADRESS POSTADRESS           
002700     03 MOD-ADGMT-LAND       PIC X(35).                                   
002800*                                 GODSMOTTAGARADRESS LAND                 
002900     03 MOD-IDPARTNR         PIC X(9).                                    
003000*                                 PARTNERNUMMER                           
003100     03 MOD-IDLEVNR          PIC X(5).                                    
003200*                                 LEVERANTÖRNUMMER                        
003300     03 MOD-KDKREDSP         PIC X.                                       
003400*                                 KREDITSPÄRR PÅ BETALARE                 
003500     03 MOD-FLCOD            PIC X.                                       
003600*                                 KONTANTBETALANDE KUND                   
003700     03 MOD-FLSAMFAK         PIC X.                                       
003800*                                 SAMFAKTURERING NEW CONCEPT              
003900     03 MOD-IDKUNDNR-HEAD    PIC Z(5)9.                                   
004000*                                 KUNDNR. TILL GMT:S HUVUDKONTOR          
004100     03 MOD-KDROPACK-DAG     PIC X.                                       
004200*                                 FRISLÄPPNINGSKOD RO/DO DAGORDER         
004300     03 MOD-BERODAG-X25      PIC X(25).                                   
004400     03 MOD-KDROPACK-BULK    PIC X.                                       
004500*                                 FRISLÄPPNINGSKOD RO/DO BULK ORD         
004600     03 MOD-BEROBULK-X25     PIC X(25).                                   
004700     03 MOD-IDGMTOMR         PIC Z(3)9.                                   
004800*                                 GODSMOTTAGAREOMRÅDE                     
004900     03 MOD-IDPRCTAB         PIC Z9.                                      
005000*                                 PRCTABELLIDENTITET                      
005100     03 MOD-IDPKLTAB         PIC X(2).                                    
005200*                                 PRODUKTIONSKLASSTABELLSID               
005300     03 MOD-INFO-RAD         OCCURS 7 TIMES.                              
005400*                                 RADINFORMATION                          
005500        05 MOD-IDDC-BULK     PIC X(2).                                    
005600*                                 IDENTIFIERARE BULKORDERLAGER            
005700        05 MOD-KDGENFRA-MO   PIC Z9.                                      
005800*                                 NORMAL FRAKT MÅNADSORDER KL 2-4         
005900        05 MOD-IDDC-DAY      PIC X(2).                                    
006000*                                 IDENTIFIERARE DAGORDERLAGER             
006100        05 MOD-KDGENFRA-DO   PIC Z9.                                      
006200*                                 NORMAL FRAKT DAGORDER                   
006300        05 MOD-IDDC-VOR      PIC X(2).                                    
006400*                                 IDENTIFIERARE VORORDERLAGER             
006500        05 MOD-KDGENFRA-VOR  PIC Z9.                                      
006600*                                 NORMAL FRAKT VOR-ORDER                  
006700     03 MOD-FLOKFAK-G        PIC X.                                       
006800*                                 FLAGGA FAKTURATYP G GODKÄND             
006900     03 MOD-FLOKFAK-K        PIC X.                                       
007000*                                 FLAGGA FAKTURATYP K GODKÄND             
007100     03 MOD-FLOKFAK-N        PIC X.                                       
007200*                                 FLAGGA FAKTURATYP N GODKÄND             
007300     03 MOD-FLOKFAK-R        PIC X.                                       
007400*                                 FLAGGA FAKTURATYP R GODKÄND             
007500     03 MOD-KDGENFAK         PIC X.                                       
007600*                                 NORMAL FAKTURATYP                       
007700     03 MOD-KVLEDTIM-0       PIC Z(2)9.9(2).                              
007800*                                 LEDTID KL 0                             
007900     03 MOD-KVLEDTIM-1       PIC Z(2)9.9(2).                              
008000*                                 LEDTID KL 1                             
008100     03 MOD-KVLEDTIM-2       PIC Z(2)9.9(2).                              
008200*                                 LEDTID KL 2                             
008300     03 MOD-KVLEDTIM-3       PIC Z(2)9.9(2).                              
008400*                                 LEDTID KL 3                             
008500     03 MOD-KVLEDTIM-4       PIC Z(2)9.9(2).                              
008600*                                 LEDTID KL 4                             
008700     03 MOD-TEMFSINF         PIC X(55).                                   
008800*                                 INFORMATIONSMEDDELANDE                  
008900*** END OF VILMAII-COPY LENGTH= 500 BYTES                                 
