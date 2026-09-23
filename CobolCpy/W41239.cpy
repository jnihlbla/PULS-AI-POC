000100 01  W41239-CTX.                                                          
000200*                                                                         
000300     03 IDORDER              PIC 9(7).                                    
000400*                                 VOLVO PARTS ORDERNUMMER                 
000500     03 IDARTNR              PIC 9(9).                                    
000600*                                 ARTIKELNUMMER                           
000700     03 IDDC                 PIC X(2).                                    
000800*                                 IDENTIFIERARE LAGER                     
000900     03 DARFS                PIC 9(12).                                   
001000*                                 KLART FÖR TRANSPORT                     
001100     03 IDDISTR              PIC 9(4).                                    
001200*                                 DISTRIKTNUMMER                          
001300     03 IDKUNDNR             PIC 9(6).                                    
001400*                                 KUNDNUMMER                              
001500     03 IDORDNR7             PIC 9(7).                                    
001600*                                 ORDERNUMMER                             
001700     03 IDSYSTEM             PIC X(4).                                    
001800*                                 VOLVO VCCS SYSTEMNUMMER                 
001900     03 KVBEART              PIC 9(6).                                    
002000*                                 BESTÄLLT ANTAL STYCKEN                  
002100     03 KVBEART-Q            PIC 9(6).                                    
002200*                                 BESTÄLLT KVANTANPASSAT ANTAL            
002300     03 KDKVBRYT             PIC 9.                                       
002400*                                 KOD OM KVANTFÖRP SKALL BRYTAS           
002500     03 KDDSP                PIC 9.                                       
002600*                                 PÅVERKAN PÅ DSP                         
002700     03 KDORDKL              PIC 9.                                       
002800*                                 ORDERKLASS                              
002900     03 BERADREF             PIC X(10).                                   
003000*                                 KUNDENS RADREFERENS                     
003100     03 KVANT-ART            PIC 9(6).                                    
003200*                                 ANTAL ARTIKLAR TOTALT                   
003300     03 IDPRQUES             PIC 9(7).                                    
003400*                                 PRISFRÅGA NR                            
003500     03 PRARTNTO-LOC         PIC 9(7)V9(2).                               
003600*                                 ARTIKELPRIS NETTO LOKAL VALUTA          
003700     03 PRARTNTO-LOCPREL     PIC 9(7)V9(2).                               
003800*                                 PREL NETTO SLUTKUNDSPRIS I              
003900*                                 LOKAL VALUTA                            
004000     03 BEKUNDRF             PIC X(15).                                   
004100*                                 KUNDENS REFERENS                        
004200     03 FLEMBORD             PIC X.                                       
004300*                                 EMBALLAGEORDER ?                        
004400     03 FLFORBI              PIC X.                                       
004500*                                 FÖRBIORDERFLAGGA                        
004600     03 FLRESTN              PIC X.                                       
004700*                                 RESTNOTERING ?                          
004800     03 FLSDCLEV             PIC X.                                       
004900*                                 LEVERANSSTYRNING SDC                    
005000     03 FLORDSPE             PIC X.                                       
005100*                                 SPECIALORDERFLAGGA                      
005200     03 FLOVRLEV             PIC X.                                       
005300*                                 ÖVERLEVERANS                            
005400     03 IDBIL.                                                            
005500*                                 BILIDENTITET                            
005600        05 IDBILTYP          PIC X(3).                                    
005700*                                 BILTYP                                  
005800        05 TIAAAA-PIE        PIC X(4).                                    
005900*                                 ÅRTAL (ÅÅÅÅ)                            
006000        05 IDCHASSI-PIE      PIC X(6).                                    
006100*                                 CHASSINUMMER PIE                        
006200     03 IDKUNDRF-RO          PIC X(10).                                   
006300*                                 KUND REF PÅ RO                          
006400     03 KDFAKTYP             PIC X.                                       
006500*                                 FAKTURATYP                              
006600     03 KDPRTYP              PIC X.                                       
006700*                                 TYP AV PRISTILLÄMPNING                  
006800     03 KDTPOTYP             PIC 9.                                       
006900*                                 TYP AV TIDPLANERAD ORDER                
007000     03 TIRODAT              PIC 9(6).                                    
007100*                                 RESTORDERDATUM         (ÅÅMMDD)         
007200     03 TITPO                PIC 9(6).                                    
007300*                                 PLANERAD ORDERDATUM                     
007400*** END OF VILMAII-COPY LENGTH= 166 BYTES                                 
