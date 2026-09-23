000100* GENERATION OF COBOL HOST STRUCTURE FROM TB1ACCE-TAB                     
000200  01 TB1ACCE.                                                             
000300*              TB1ACCE                                                    
000400   03 IDARTNR         PIC S9(9) COMP-3.                                   
000500*              ARTIKELNUMMER                                              
000600   03 BEART           PIC X(25).                                          
000700*              ARTIKELBENÄMNING                                           
000800   03 IDPRODGR        PIC X(4).                                           
000900*              PRODUKTGRUPP CHEF                                          
001000   03 IDUPPDSU        PIC X(8).                                           
001100*              SU-UPPDRAGSNUMMER TIKO                                     
001200   03 BEANST-SU       PIC X(25).                                          
001300*              SU-ANSVARIGS NAMN                                          
001400   03 IDUPPDKU        PIC X(8).                                           
001500*              KU-UPPDRAGSNUMMER TIKO                                     
001600   03 BEANST-KU       PIC X(25).                                          
001700*              KU-ANSVARIGS NAMN                                          
001800   03 IDFKNGRP        PIC S9(5) COMP-3.                                   
001900*              FUNKTIONSGRUPP                                             
002000   03 IDAOT           PIC X(6).                                           
002100*              ÄNDRINGSORDERNUMMER T                                      
002200   03 IDAOTUTG        PIC S9(3) COMP-3.                                   
002300*              ÄNDRINGSORDER-T UTGÅVA                                     
002400   03 TIAOINF         PIC S9(7) COMP-3.                                   
002500*              DATUM ÄO-INFÖRANDE                                         
002600   03 BEASSTYP        PIC X(7).                                           
002700*              TILLBEHÖR TILLDELNINGSTYP                                  
002800   03 KDARTTYP        PIC X(1).                                           
002900*              TYP AV ARTIKEL                                             
003000   03 VKART-KDP       PIC S9(9) COMP-3.                                   
003100*              ARTIKELVIKT KDP (G)                                        
003200   03 KDMDS           PIC X(1).                                           
003300*              MATERIALDATAKOD                                            
003400   03 KDFARGST        PIC X(1).                                           
003500*              FÄRGSTATUS                                                 
003600   03 KDFRPTYP        PIC X(1).                                           
003700*              TYP AV FÖRPACKNING                                         
003800   03 TEARTUTFG       PIC X(8).                                           
003900*              ARTIKELUTF.GILTIGHET KDP                                   
004000   03 TESTATUPP       PIC X(6).                                           
004100*              UPPDRAGSNUMMER STATUS                                      
004200   03 IDLEVNR-GSDB    PIC X(5).                                           
004300*              LEVERANTÖR GSDB                                            
004400   03 KDTPD-PH1       PIC X(1).                                           
004500*              KOD TILLF. UTF.PROV PH1                                    
004600   03 DATPDPH1        PIC X(6).                                           
004700*              TPD-VECKA PHASE1 (AAAAVV)                                  
004800   03 DAPSWQP-1       PIC X(6).                                           
004900*              PSW-P-QUAL-PHASE1(AAAAVV)                                  
005000   03 KDPSWQP-1       PIC X(1).                                           
005100*              STATUSKOD PLAN QUAL PH1                                    
005200   03 DAPSWQA-1       PIC X(6).                                           
005300*              PSW-A-QUAL-PHASE1(AAAAVV)                                  
005400   03 KDPSWQA-1       PIC X(1).                                           
005500*              STATUSKOD AKT QUAL PH1                                     
005600   03 DAPSWPP-2       PIC X(6).                                           
005700*              PSW-P-PROD-PHASE2(AAAAVV)                                  
005800   03 KDPSWPP-2       PIC X(1).                                           
005900*              STATUSKOD PLAN PROD PH2                                    
006000   03 DAPSWPA-2       PIC X(6).                                           
006100*              PSW-A-PROD-PHASE2(AAAAVV)                                  
006200   03 KDPSWPA-2       PIC X(1).                                           
006300*              STATUSKOD AKT PROD PH2                                     
006400   03 DAPSWCP-3       PIC X(6).                                           
006500*              PSW-P-CAP-PHASE3 (AAAAVV)                                  
006600   03 KDPSWCP-3       PIC X(1).                                           
006700*              STATUSKOD PLAN CAP PH3                                     
006800   03 DAPSWCA-3       PIC X(6).                                           
006900*              PSW-A-CAP-PHASE3 (AAAAVV)                                  
007000   03 KDPSWCA-3       PIC X(1).                                           
007100*              STATUSKOD AKT CAP PH3                                      
007200   03 KVYVOL-B3       PIC S9(7) COMP-3.                                   
007300*              JUST.AV ÅRSVOLYM BOARD 3                                   
007400   03 KVYVOL-B2       PIC S9(7) COMP-3.                                   
007500*              JUST.AV ÅRSVOLYM BOARD 2                                   
007600   03 KVYVOL          PIC S9(7) COMP-3.                                   
007700*              BESL.ÅRSVOLYM UTOM "LAUNCH"                                
007800   03 KVYVOL-INT      PIC S9(7) COMP-3.                                   
007900*              BESL. ÅRSVOL FÖR INTROD.                                   
008000   03 KVYVOL-B1       PIC S9(7) COMP-3.                                   
008100*              JUSTERAD ÅRSVOLYM BOARD 1                                  
008200   03 KVYVOL-ASS      PIC S9(7) COMP-3.                                   
008300*              ÅSATT ÅRSVOLYM I LAGER                                     
008400   03 KVMAM-N         PIC S9(5) COMP-3.                                   
008500*              INTROD MARKNAD USA                                         
008600   03 KVMAM-E         PIC S9(5) COMP-3.                                   
008700*              INTROD NORDIC, EU                                          
008800   03 KVMAM-O         PIC S9(5) COMP-3.                                   
008900*              INTROD. MARKN ASIEN                                        
009000   03 BEMAPP          PIC X(5).                                           
009100*              MAPP                                                       
009200   03 KVFOTO          PIC S9(3) COMP-3.                                   
009300*              ANTAL FOTOGRAFIER                                          
009400   03 TIFOTO          PIC S9(5) COMP-3.                                   
009500*              FOTO MTRL VECKA                                            
009600   03 TENOTE          PIC X(40).                                          
009700*              NOTERINGSFÄLT                                              
009800   03 FLANNULL        PIC X(1).                                           
009900*              ANNULLATION                                                
010000   03 TEVERKTYG       PIC X(4).                                           
010100*              VERKTYGSKÖP INFO                                           
010200   03 TESTATXT        PIC X(25).                                          
010300*              STA INFORMATION                                            
010400   03 TEMATXT         PIC X(25).                                          
010500*              MA INFORMATION                                             
010600   03 TEINKTXT        PIC X(25).                                          
010700*              INK INFORMATION                                            
010800   03 TEANSTXT        PIC X(25).                                          
010900*              ANSK INFORMATION                                           
011000   03 TEAUXTXT        PIC X(25).                                          
011100*              EXTRA NOTERING                                             
011200   03 IDKDPPOS        PIC S9(3) COMP-3.                                   
011300*              ARTIKELPOS I KDP                                           
011400   03 IDPSLAG         PIC X(2).                                           
011500*              PSLAG IDENTITET KDP                                        
011600   03 IDTYPNR         PIC X(2).                                           
011700*              TYPNR IDENTITET KDP                                        
011800   03 BETYP           PIC X(8).                                           
011900*              KDP TYP                                                    
012000   03 IDARTNR-OFARG   PIC S9(9) COMP-3.                                   
012100*              OFARGAD ARTIKEL KDP                                        
012200   03 IDPROJK         PIC X(4).                                           
012300*              PROJEKTIDENTITET KONSTRUKTION                              
012400   03 IDPSS           PIC X(5).                                           
012500*              PSS IDENTITET KDP                                          
012600   03 KDANNULL        PIC X(1).                                           
012700*              CANCELLATION CODE                                          
012800   03 BEUPPDSU        PIC X(35).                                          
012900*              SU-UPPDRAG BENÄMNING                                       
013000   03 BEUPPDKU        PIC X(35).                                          
013100*              KU-UPPDRAG BENÄMNING                                       
013200*                                                                         
013300*** END OF VILMAII-COPY LENGTH= 511 OLD LENGTH=                           
