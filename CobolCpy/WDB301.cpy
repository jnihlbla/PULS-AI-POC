000100 01  DC-WDB301.                                                           
000200*                                 KUNDREGISTER                            
000300*                                 LAGER INFO                              
000400*                                 FYSISK NYCKEL: WDB301KY                 
000500*                                 (IDDC + IDDISTR + IDKUNDNR)             
000600     03 DC-IDDC              PIC X(2).                                    
000700*                                 IDENTIFIERARE LAGER                     
000800*                                 WAREHOUSE IDENTIFIER                    
000900     03 DC-IDGMT.                                                         
001000*                                 GODSMOTTAGARE                           
001100*                                 GOODS RECEIVER                          
001200        05 DC-IDDISTR        PIC S9(5)           COMP-3.                  
001300*                                 DISTRIKTNUMMER                          
001400*                                 DISTRICT NUMBER                         
001500        05 DC-IDKUNDNR       PIC S9(7)           COMP-3.                  
001600*                                 KUNDNUMMER                              
001700*                                 CUSTOMER NO                             
001800     03 DC-IDGMTOMR          PIC 9(4).                                    
001900*                                 GODSMOTTAGAREOMRÅDE                     
002000*                                 GOODS RECEIVING AREA                    
002100     03 DC-IDPKLTAB          PIC X(2).                                    
002200*                                 PRODUKTIONSKLASSTABELLSID               
002300*                                 PRODUCTION CLASS TABLE ID               
002400     03 DC-IDPRCTAB          PIC 9(2).                                    
002500*                                 PRCTABELLIDENTITET                      
002600*                                 PRC TABLE IDENTITY                      
002700     03 DC-KDFORSKN          PIC S9(3)           COMP-3.                  
002800*                                 FÖRSÄKRANSKOD                           
002900*                                 DECLARATION CODE                        
003000     03 DC-KDGENFRA-DO       PIC S9(3)           COMP-3.                  
003100*                                 NORMAL FRAKT DAGORDER                   
003200*                                 NORMAL FC CLASS 1                       
003300     03 DC-KDGENFRA-MO       PIC S9(3)           COMP-3.                  
003400*                                 NORMAL FRAKT MÅNADSORDER KL 2-4         
003500*                                 NORMAL FC CLASS 2-4                     
003600     03 DC-KDGENFRA-VOR      PIC S9(3)           COMP-3.                  
003700*                                 NORMAL FRAKT VOR-ORDER                  
003800*                                 NORMAL FC VOR-ORDER                     
003900     03 DC-KDMOMSIN          PIC S9              COMP-3.                  
004000*                                 MOMSINSTRUKTION                         
004100*                                 TVA-INSTRUCTION                         
004200     03 DC-KDROPACK-BULK     PIC X.                                       
004300*                                 FRISLÄPPNINGSKOD RO/DO BULK ORD         
004400*                                 CONSOLIDATION BO/DO BULKORDER           
004500     03 DC-KDROPACK-DAG      PIC X.                                       
004600*                                 FRISLÄPPNINGSKOD RO/DO DAGORDER         
004700*                                 CONSOLIDATION BO/DO DAYORDER            
004800     03 DC-KDSPFKTK          PIC S9(3)           COMP-3.                  
004900*                                 INSTRUKTION SPECIALFAKTURA              
005000     03 DC-KDTULLVE          PIC S9              COMP-3.                  
005100*                                 TYP AV PRIS PÅ TULLFAKTURA              
005200*                                 TYPE OF PRICE ON CUSTOMS INVOIC         
005300*                                 E                                       
005400     03 DC-KVDAGAR-TRP-DAY   PIC S9(3)           COMP-3.                  
005500*                                 TRP DAGAR DC TILL KUND (DAG)            
005600*                                 TRANSP.DAYS DC TO CUST (DAY)            
005700     03 DC-KVLEDTIM-0        PIC S9(3)V9(2)      COMP-3.                  
005800*                                 LEDTID KL 0                             
005900*                                 LEAD TIME CL 0                          
006000     03 DC-KVLEDTIM-1        PIC S9(3)V9(2)      COMP-3.                  
006100*                                 LEDTID KL 1                             
006200*                                 LEAD TIME CL 1                          
006300     03 DC-KVLEDTIM-2        PIC S9(3)V9(2)      COMP-3.                  
006400*                                 LEDTID KL 2                             
006500*                                 LEAD TIME CL 2                          
006600     03 DC-KVLEDTIM-3        PIC S9(3)V9(2)      COMP-3.                  
006700*                                 LEDTID KL 3                             
006800*                                 LEAD TIME CL 3                          
006900     03 DC-KVLEDTIM-4        PIC S9(3)V9(2)      COMP-3.                  
007000*                                 LEDTID KL 4                             
007100*                                 LEAD TIME CL 4                          
007200*** END OF VILMAII-COPY LENGTH= 48 BYTES                                  
