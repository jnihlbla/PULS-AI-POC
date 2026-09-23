000100 01  W4832102.                                                            
000200*                                 CROSS DOCK DATA TO DATA                 
000300*                                 LAKE                                    
000400     03 IDDC-CROSS           PIC X(2).                                    
000500*                                 DC FÖR CROSS DOCKING                    
000600*                                 DC FOR CROSS DOCKING                    
000700     03 TAB-1                PIC X.                                       
000800*                                 TAB-TECKEN                              
000900*                                 TAB-CHARACTER                           
001000     03 TIRFSDAT             PIC 9(6).                                    
001100*                                 KLART FÖR TRANSPORT ÅÅMMDD              
001200*                                 READY FOR SHIPMENT  YYMMDD              
001300     03 TAB-2                PIC X.                                       
001400*                                 TAB-TECKEN                              
001500*                                 TAB-CHARACTER                           
001600     03 IDDISTR              PIC Z(3)9.                                   
001700*                                 DISTRIKTNUMMER                          
001800*                                 DISTRICT NUMBER                         
001900     03 TAB-3                PIC X.                                       
002000*                                 TAB-TECKEN                              
002100*                                 TAB-CHARACTER                           
002200     03 IDKUNDNR             PIC Z(5)9.                                   
002300*                                 KUNDNUMMER                              
002400*                                 CUSTOMER NO                             
002500     03 TAB-4                PIC X.                                       
002600*                                 TAB-TECKEN                              
002700*                                 TAB-CHARACTER                           
002800     03 IDPRODNR             PIC Z(6)9.                                   
002900*                                 PRODUKTIONSNUMMER                       
003000*                                 PRODUCTION NUMBER                       
003100     03 TAB-5                PIC X.                                       
003200*                                 TAB-TECKEN                              
003300*                                 TAB-CHARACTER                           
003400     03 IDKOLLI              PIC Z(4)9.                                   
003500*                                 KOLLINUMMER                             
003600*                                 CASE NUMBER                             
003700     03 TAB-6                PIC X.                                       
003800*                                 TAB-TECKEN                              
003900*                                 TAB-CHARACTER                           
004000     03 IDLEVNR              PIC X(5).                                    
004100*                                 LEVERANTÖRNUMMER                        
004200*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
004300     03 TAB-7                PIC X.                                       
004400*                                 TAB-TECKEN                              
004500*                                 TAB-CHARACTER                           
004600     03 IDSUPREF             PIC X(10).                                   
004700*                                 LEVERANTöRSREF.                         
004800*                                 SUPPLIER REF.                           
004900     03 TAB-8                PIC X.                                       
005000*                                 TAB-TECKEN                              
005100*                                 TAB-CHARACTER                           
005200     03 IDTRPTNR-CROSS       PIC X(3).                                    
005300*                                 TRANSPORTIDENTITET DCCROSS              
005400*                                 TRANSPORT IDENTITY DCCROSS              
005500     03 TAB-9                PIC X.                                       
005600*                                 TAB-TECKEN                              
005700*                                 TAB-CHARACTER                           
005800     03 TIRECXDAT            PIC Z(6).                                    
005900*                                 DATUM FÖR MOTTAG KLI I KROSS-DC         
006000*                                 DATE CASE BINNING IN CROSS-DC           
006100     03 TAB-10               PIC X.                                       
006200*                                 TAB-TECKEN                              
006300*                                 TAB-CHARACTER                           
006400     03 TIRECXTID            PIC Z(4).                                    
006500*                                 TID FÖR MOTTAG KOLLI I KROSS-DC         
006600*                                 TIME CASE BINNING IN CROSS-DC           
006700     03 TAB-11               PIC X.                                       
006800*                                 TAB-TECKEN                              
006900*                                 TAB-CHARACTER                           
007000     03 IDDC-SEND            PIC X(2).                                    
007100*                                 SÄNDANDE LAGER                          
007200*                                 SENDING WAREHOUSE                       
007300     03 TAB-12               PIC X.                                       
007400*                                 TAB-TECKEN                              
007500*                                 TAB-CHARACTER                           
007600     03 KDKOLSTA-CROSS       PIC 9.                                       
007700*                                 KOLLISTATUS CROSS-DOCK KOLLI            
007800*                                 CASE STATUS FOR CROSS-DOCK CASE         
007900     03 TAB-13               PIC X.                                       
008000*                                 TAB-TECKEN                              
008100*                                 TAB-CHARACTER                           
008200     03 TISKEPPN             PIC 9(6).                                    
008300*                                 SKEPPNINGSDATUM  (ÅÅMMDD)               
008400*                                 SHIPPING DATE    (YYMMDD)               
008500     03 TAB-14               PIC X.                                       
008600*                                 TAB-TECKEN                              
008700*                                 TAB-CHARACTER                           
008800     03 IDSHIPM-CROSS        PIC Z(6)9.                                   
008900*                                 SKEPPNINGSNUMMER FRÅN KROSS DC          
009000*                                 SHIPMENT NO ON A CROSS DC               
009100     03 TAB-15               PIC X.                                       
009200*                                 TAB-TECKEN                              
009300*                                 TAB-CHARACTER                           
009400     03 IDLBBET-CROSS        PIC X(12).                                   
009500*                                 LASTBÄRARBETECKNING DCCROSS             
009600*                                 TRAILER NUMBER                          
009700*** END OF VILMAII-COPY LENGTH= 101 BYTES                                 
