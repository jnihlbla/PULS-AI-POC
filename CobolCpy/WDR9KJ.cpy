010004 01  EKH-WDR901.                                                          
020000*                                 EKONOMISKA HÄNDELSETRANSAR              
030000*                                 FYSISK NYCKEL: WDR901KY                 
040000*                                 (IDPGM + DAREGDAT + TIKLOCK +           
050000*                                  IDSEKVNR + IDCPYTXT                    
060000*                                 SÖKBEGREPP: IDPGM, DAREGDAT,            
070000*                                 TIKLOCK, IDSEKVNR, IDCPYTXT             
080004     03 EKH-IDPGM            PIC X(8).                                    
090000*                                 PROGRAM IDENTITET                       
110004     03 EKH-DAREGDAT         PIC 9(8).                                    
120000*                                 REGISTRERINGSDATUM (ÅÅÅÅMMDD)           
140004     03 EKH-TIKLOCK          PIC S9(9)           COMP-3.                  
150000*                                 KLOCKSLAG (TTMMSSTH)                    
170004     03 EKH-IDSEKVNR         PIC S9(3)           COMP-3.                  
180000*                                 GENERELLT SEKVENSNUMMER                 
200004     03 EKH-IDCPYTXT         PIC X(8).                                    
210000*                                 COPYTEXT IDENTITET                      
320004     03 EKH-IDUSER           PIC X(8).                                    
330000*                                 ANVÄNDARENS SÄKERHETS ID                
351001     03 EKH-BEVAT            PIC X(2).                                    
352001*                                 MOMSKODSBENÄMNING                       
353001     03 EKH-DAVERDAT         PIC 9(8).                                    
354001*                                 VERIFIKATIONSDATUM (ÅÅÅÅMMDD)           
355001     03 EKH-FLLSBOK          PIC X.                                       
356001*                                 LAGERAVBOKNING                          
357001     03 EKH-IDANALYS         PIC X(12).                                   
358001*                                 ANALYSNUMMER                            
359001     03 EKH-IDARTNR          PIC S9(9)           COMP-3.                  
359101*                                 ARTIKELNUMMER                           
359201     03 EKH-IDDC-SEND        PIC X(2).                                    
359301*                                 SÄNDANDE LAGER                          
359401     03 EKH-IDDC-REC         PIC X(2).                                    
359501*                                 MOTTAGANDE LAGER                        
359601     03 EKH-IDDISTR          PIC S9(5)           COMP-3.                  
359701*                                 DISTRIKTNUMMER                          
359801     03 EKH-IDKONTO          PIC S9(11)          COMP-3.                  
359901*                                 KONTO                                   
360001     03 EKH-IDKST            PIC 9(5).                                    
361001*                                 KOSTNADSSTÄLLE                          
362001     03 EKH-IDKUNDNR         PIC S9(7)           COMP-3.                  
363001*                                 KUNDNUMMER                              
364001     03 EKH-IDTRANS          PIC X(4).                                    
365001*                                 BILDNUMMER                              
366001     03 EKH-IDVERGL          PIC X(10).                                   
367001*                                 VERIFIKATIONSID FÖR HUVUDBOKEN          
368001     03 EKH-KDANMORS         PIC X(2).                                    
369001*                                 ORSAK TILL LEVERANSANMÄRKNING           
369301     03 EKH-KDEKHHT          PIC X(3).                                    
369401*                                 EKONOMISK HUVUDHÄNDELSE                 
369501     03 EKH-KDEKSHT          PIC X(3).                                    
369601*                                 EKONOMISK SUBHÄNDELSE                   
369701     03 EKH-KDEKNIVA         PIC X(5).                                    
369801*                                 EKONOMISK HÄNDELSENIVÅ                  
369901     03 EKH-KDFRAKT          PIC S9(3)           COMP-3.                  
370001*                                 FRAKTSÄTT DC TILL KUND                  
371001     03 EKH-KDPRODSL         PIC S9(3)           COMP-3.                  
372001*                                 PRODUKTSLAG                             
373001     03 EKH-KDPSLLOC         PIC 9(2).                                    
374001*                                 PRODUKTSLAG LOKALT                      
375001     03 EKH-KDVALISO         PIC X(3).                                    
376001*                                 VALUTAKOD ENLIGT ISO-STANDARD.          
377001     03 EKH-KVANTAL          PIC S9(7)           COMP-3.                  
378001*                                 ANTAL                                   
379001     03 EKH-PRARTNTO         PIC S9(7)V9(2)      COMP-3.                  
379101*                                 ARTIKELPRIS NETTO                       
379201     03 EKH-PRARTSJK         PIC S9(7)V9(2)      COMP-3.                  
379301*                                 ARTIKELNS SJÄLVKOSTNAD                  
379601     03 EKH-PRARTSTD         PIC S9(7)V9(2)      COMP-3.                  
379701*                                 ARTIKELSTANDARDPRIS                     
379801     03 EKH-PRDIRLON         PIC S9(4)V9(3)      COMP-3.                  
379901*                                 DIREKT LÖN                              
380001     03 EKH-PRDMTRL          PIC S9(6)V9(3)      COMP-3.                  
380101*                                 DIREKT MATERIAL                         
380201     03 EKH-PRINK            PIC S9(7)V9(2)      COMP-3.                  
381001*                                 INKÖPSPRIS                              
382001     03 EKH-PRKURS           PIC S9(6)V9(5)      COMP-3.                  
383001*                                 VALUTAKURS                              
384001     03 EKH-PRLANDCO         PIC S9(7)V9(2)      COMP-3.                  
385001*                                 LANDING COST                            
386001     03 EKH-PROVRPAL         PIC S9(4)V9(3)      COMP-3.                  
387001*                                 ÖVRIGA OMKOSTNADER PÅLÄGG               
388001     03 EKH-SUBEL            PIC S9(9)V9(2).                              
389001*                                 SUMMABELOPP                             
389101     03 EKH-SUVAT            PIC S9(11)V9(2)     COMP-3.                  
389201*                                 MOMSVÄRDE PER MOMSKOD                   
389301     03 EKH-DAAVIDAT         PIC 9(8).                                    
389401*                                 AVISERINGSDATUM (YYYYMMDD)              
389501     03 EKH-IDAVINR          PIC S9(7)           COMP-3.                  
389601*                                 AVI-NUMMER                              
389903     03 EKH-IDLEVNR          PIC X(5).                                    
390001*                                 LEVERANTÖRNUMMER                        
390101     03 EKH-KDAVVTYP         PIC S9              COMP-3.                  
390201*                                 AVVIKELSETYP                            
391001*                                 1=POSITIV.  2=NEGATIV                   
392001     03 EKH-KDRT             PIC S9(3)           COMP-3.                  
393001*                                 REDOVISNINGSTYP                         
394001     03 EKH-KVANTMOT         PIC S9(7)           COMP-3.                  
395001*                                 ANTAL MOTTAGET                          
396001     03 EKH-KVAVIS           PIC S9(7)           COMP-3.                  
397001*                                 AVISERAT ANTAL                          
398001     03 EKH-KDSORT           PIC X(2).                                    
399001*                                 SORT-KOD                                
399101     03 EKH-KDTRADP          PIC X(4).                                    
399201*                                 TRADING PARTNER                         
399301     03 EKH-FLOVRLEV         PIC X.                                       
399401*                                 ÖVERLEVERANS                            
399501     03 EKH-IDORDNR5         PIC S9(5)           COMP-3.                  
399601*                                 ORDERNUMMER                             
399705     03 EKH-IDUSER2          PIC X(8).                                    
399802*                                 ANVÄNDARENS SÄKERHETS ID                
399904     03 EKH-IDREF            PIC X(15).                                   
400002*                                 REFERENS ID                             
400104     03 EKH-BEFELSAP         PIC X(20).                                   
400202*                                 FELTEXT FÖR SAP-TRANSAKTIO              
400304     03 EKH-FLKLAR           PIC X.                                       
400402*                                 AVSLUTNINGSMARKERING                    
400504     03 EKH-PRHEMTAG         PIC S9(7)V9(2)      COMP-3.                  
400602*                                 HEMTAGNINGSKOSTNAD                      
400603     03 EKH-FLDCET           PIC X(1).                                    
400604*                                 DC 21 EXCHANGE TERMINAL                 
401002     03 EKH-IDKUNDRF         PIC X(10).                                   
401003*                                 KUNDENS REFERENS (ORDERID)              
410000*** END OF VILMAII-COPY LENGTH= 289 BYTES                                 
