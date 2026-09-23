000010 01  ODEL-W41302.                                                         
000020*                                 WOPS - OUTPUT FROM W41302-PGM           
000030     03 ODEL-IDORDER         PIC S9(7)           COMP-3.                  
000040*                                 VOLVO PARTS ORDERNUMMER                 
000050*                                 VOLVO PARTS ORDER NUMBER                
000060     03 ODEL-IDDC            PIC X(2).                                    
000070*                                 IDENTIFIERARE LAGER                     
000080*                                 WAREHOUSE IDENTIFIER                    
000090     03 ODEL-IDPRODNR        PIC S9(7)           COMP-3.                  
000100*                                 PRODUKTIONSNUMMER                       
000110*                                 PRODUCTION-NUMBER                       
000120     03 ODEL-IDPLKLST        PIC S9(3)           COMP-3.                  
000130*                                 PLOCKLISTNUMMER                         
000140*                                 PICKING LIST NUMBER                     
000150     03 ODEL-IDBORD          PIC X(3).                                    
000160*                                 PACK-BORD                               
000170*                                 PACKING TABLE                           
000180     03 ODEL-IDGMTREF.                                                    
000190*                                 GODSMOTTAGAREREFERENS                   
000200*                                 GOODS RECEIVER REFERENS                 
000210        05 ODEL-IDDISTR      PIC S9(5)           COMP-3.                  
000220*                                 DISTRIKTNUMMER                          
000230*                                 DISTRICT NUMBER                         
000240        05 ODEL-IDKUNDNR     PIC S9(7)           COMP-3.                  
000250*                                 KUNDNUMMER                              
000260*                                 CUSTOMER NO                             
000270        05 ODEL-IDKUNDRF     PIC X(10).                                   
000280*                                 KUNDENS REFERENS (ORDERID)              
000290*                                 CUSTOMER REFERENCE (ORDER ID)           
000300        05 ODEL-IDORDNR5-FILLER REDEFINES ODEL-IDKUNDRF.                  
000310           07 ODEL-IDORDNR5  PIC 9(5).                                    
000320*                                 ORDERNUMMER                             
000330*                                 ORDER NUMBER                            
000340           07 FILLER         PIC X(5).                                    
000350        05 ODEL-IDORDNR7-FILLER REDEFINES ODEL-IDKUNDRF.                  
000360           07 ODEL-IDORDNR7  PIC 9(7).                                    
000370*                                 ORDERNUMMER                             
000380*                                 ORDER NUMBER                            
000390           07 FILLER         PIC X(3).                                    
000400     03 ODEL-IDLEVNR         PIC S9(5)           COMP-3.                  
000410*                                 LEVERANTÖRNUMMER                        
000420*                                 SUPPLIER NUMBER                         
000430     03 ODEL-IDPRC.                                                       
000440*                                 PRODUKTIONSKANAL                        
000450*                                 PRODUCTION CHANNEL                      
000460        05 ODEL-IDPRCBAS     PIC X(3).                                    
000470*                                 PRC-BAS                                 
000480*                                 PRC-BASIC                               
000490        05 ODEL-IDPRCVAR     PIC X.                                       
000500*                                 PRC-VARIANT                             
000510*                                 PRC-VARIANT                             
000520     03 ODEL-IDUSER          PIC X(8).                                    
000530*                                 ANVÄNDARENS SÄKERHETS ID                
000540*                                 USER SECURITY-IDENTITY                  
000550     03 ODEL-KDFDKRAV        PIC S9(3)           COMP-3.                  
000560*                                 TRANSPORTFÖRPACKNINGSKOD                
000570*                                 PACKING CODE                            
000580     03 ODEL-KDODELSTA       PIC X.                                       
000590*                                 ORDERDELSTATUS                          
000600*                                 ORDER PART STATUS                       
000610     03 ODEL-KDPRODKL        PIC X.                                       
000620*                                 PRODUKTIONSKLASS                        
000630*                                 PRODUCTION CLASS                        
000640     03 ODEL-KVART           PIC S9(7)           COMP-3.                  
000650*                                 ANTAL ARTNR PER BRYTBEGREPP             
000660*                                 NO OF PARTNOS PER TYPE                  
000670     03 ODEL-KVPACKRAD-OD    PIC S9(5)           COMP-3.                  
000680*                                 ANTAL PACKADE RADER                     
000690*                                 NUMBER OF PACKED RADER                  
000700     03 ODEL-KVPTID          PIC S9(2)V9(1)      COMP-3.                  
000710*                                 GENOMSNITTLIG TID/RAD MINUTER           
000720*                                 AVERIDGE TIME/LINE MINUTES              
000730     03 ODEL-KVRADER         PIC S9(5)           COMP-3.                  
000740*                                 ANTAL RADER                             
000750*                                 NUMBER OF LINES                         
000760     03 ODEL-SUHANTTI        PIC S9(7)           COMP-3.                  
000770*                                 SUMMA HANTERINGSKOD TID                 
000780*                                 TOTAL PIECEWORK TIME                    
000790     03 ODEL-SUORDV          PIC S9(9)V9(2)      COMP-3.                  
000800*                                 SUMMA ORDERVÄRDE                        
000810*                                 TOTAL ORDER VALUE                       
000820     03 ODEL-SUPTID          PIC S9(3)V9(2)      COMP-3.                  
000830*                                 TOTAL PRODUKTIONSTID TIM+MIN            
000840*                                 TOTAL PRODUCTIONTIME HOUR MIN.          
000850     03 ODEL-TILST-OD        PIC S9(11)          COMP-3.                  
000860*                                 SENASTE STARTTIDPUNKT ORDERDEL          
000870*                                 LATEST START-TIME ORDER-PART            
000880     03 ODEL-TIPACKN         PIC S9(7)           COMP-3.                  
000890*                                 PACKNINGSDATUM         (ÅÅMMDD)         
000900*                                 PACKING DATE           (YYMMDD)         
000910     03 ODEL-TIPACTID        PIC S9(7)           COMP-3.                  
000920*                                 PACKNINGSTID  TTMMSS                    
000930*                                 PACKING TIME  HHMMSS                    
000940     03 ODEL-TIREGDAT        PIC S9(7)           COMP-3.                  
000950*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
000960*                                 REGISTRATION DATE (YYMMDD)              
000970     03 ODEL-TIREGTID        PIC S9(7)           COMP-3.                  
000980*                                 REGISTRERINGSTID                        
000990*                                 GENERAL REGISTRATION TIME               
001000     03 ODEL-VKORDNTO        PIC S9(6)V9(1)      COMP-3.                  
001010*                                 ORDERVIKT NETTO (KG)                    
001020*                                 WEIGHT PER ORDER NETTO (KG)             
001030     03 ODEL-VLORDNTO        PIC S9(4)V9(3)      COMP-3.                  
001040*                                 ORDERVOLYM NETTO (M3)                   
001050*                                 NET VOLUME PER ORDER (M3)               
001060     03 ODEL-IDTRP.                                                       
001070*                                 TRANSPORTIDENTITET                      
001080*                                 TRANSPORTIDENTITY                       
001090        05 ODEL-IDTRPLOS     PIC X(3).                                    
001100*                                 TRANSPORTLÖSNING                        
001110*                                 TRANSPORTSOLUTION                       
001120        05 ODEL-IDTRPVAR     PIC X(2).                                    
001130*                                 TRANSPORTLÖSNINGSGRUPP                  
001140*                                 TRANSPORTSOLUTIONGROUP                  
001150     03 ODEL-TITRPAVT.                                                    
001160*                                 TRANSPORTAVGÅNGSTIDPUNKT                
001170*                                 TRANSPORT DEPARTURE                     
001180        05 ODEL-TIAAMMDD     PIC S9(7)           COMP-3.                  
001190*                                 ÅR - MÅNAD - DAG  (ÅÅMMDD)              
001200*                                 YEAR - MONTH - DAY  (YYMMDD)            
001210        05 ODEL-TIHHMM       PIC S9(5)           COMP-3.                  
001220*                                 KLOCKSLAG (TIMMAR OCH MINUTER)          
001230*                                 TIME IN HOUR AND MINUTE                 
001240     03 ODEL-TIUTSKR         PIC S9(7)           COMP-3.                  
001250*                                 UTSKRIFTDATUM  (ÅÅMMDD)                 
001260*                                 PRINTING DATE  (YYMMDD)                 
001270     03 ODEL-TIUTSTID        PIC S9(7)           COMP-3.                  
001280*                                 UTSKRIFTSTID (TTMMSS)                   
001290*                                 TIME OF PRINTING (HHMMSS)               
001300     03 ODEL-TIRFS           PIC S9(11)          COMP-3.                  
001310*                                 KLART FÖR TRANSPORT ÅÅMMDDTTMM          
001320*                                 READY FOR SHIPMENT  YYMMDDHHMM          
001330     03 ODEL-TILST-O         PIC S9(11)          COMP-3.                  
001340*                                 SENASTE STARTTIDPUNKT FÖR ORDER         
001350*                                 LATEST START-TIME ORDER                 
      *** END COPY W41302      LENGTH=138                                       
