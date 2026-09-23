000100 01  W61110.                                                              
000200*                                 URVAL FRÅN W6D1                         
000300*                                 SELECTED EXTRACT FROM W6D1              
000400*                                                           .             
000500     03 INL-FLFEL            PIC X.                                       
000600*                                 ALLMÄN FELFLAGGA                        
000700*                                 GENERAL ERROR FLAG                      
000800     03 IDDC                 PIC X(2).                                    
000900*                                 IDENTIFIERARE LAGER                     
001000*                                 WAREHOUSE IDENTIFIER                    
001100     03 IDLEVNR              PIC X(5).                                    
001200*                                 LEVERANTÖRNUMMER                        
001300*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
001400     03 IDFS                 PIC X(8).                                    
001500*                                 FÖLJESEDELSNUMMER ENL ODETTE            
001600*                                 ADVICE NOTE NUMBER ODETTE               
001700     03 TIAVIDAT             PIC S9(7)           COMP-3.                  
001800*                                 AVISERINGSDATUM (YYMMDD)                
001900*                                 ADVICE NOTE DATE                        
002000     03 IDLBBET              PIC X(12).                                   
002100*                                 LASTBÄRARBETECKNING                     
002200*                                 TRAILER NUMBER                          
002300     03 KDINL                PIC X(3).                                    
002400*                                 TYP AV INLEVERANS                       
002500*                                 TYPE OF INC.DELIVERY                    
002600     03 TIANKDAG             PIC S9(7)           COMP-3.                  
002700*                                 ANKOMSTDAG                              
002800*                                 RECEIVING DATE                          
002900     03 TIINLMOT             PIC S9(7)           COMP-3.                  
003000*                                 MOTTAGNINGSDATUM   (ÅÅMMDD)             
003100*                                 RECEIVING DATE    (YYMMDD)              
003200     03 IDARTNR              PIC S9(9)           COMP-3.                  
003300*                                 ARTIKELNUMMER                           
003400*                                 PART NUMBER                             
003500     03 IDRADNR-INL          PIC S9(5)           COMP-3.                  
003600*                                 RADNUMMER INLEVERANS                    
003700*                                 LINE NUMBER GOODS RECEIVING             
003800     03 BEFT                 PIC S9(3)           COMP-3.                  
003900*                                 FÖRPACKNINGSTYP                         
004000*                                 PACKAGING TYPE                          
004100     03 FLETIKETT            PIC X.                                       
004200*                                 ANGER ATT ETIKETT ÄR PRINTAD            
004300*                                 PRE-PACKING LABEL ARE PRINTED           
004400     03 ART-FLFEL            PIC X.                                       
004500*                                 ALLMÄN FELFLAGGA                        
004600*                                 GENERAL ERROR FLAG                      
004700     03 FLKLAR               PIC X.                                       
004800*                                 AVSLUTNINGSMARKERING                    
004900*                                 FINISHED FLAG                           
005000     03 FLKVAFEL             PIC X.                                       
005100*                                 FLAGGA KVALITETSFEL                     
005200*                                 FLAG QUALITY ERROR                      
005300     03 FLKVAKAR             PIC X.                                       
005400*                                 FLAGGA KARANTÄN                         
005500*                                 FLAG QUARANTINE                         
005600     03 IDLOPNRM             PIC S9(9)           COMP-3.                  
005700*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
005800*                                 (0VVDLLLLK)                             
005900*                                 SERIAL NO RECEIVING REPORT              
006000*                                 (0WWDLLLLC)                             
006100     03 ART-KDINLPRIO        PIC S9(3)           COMP-3.                  
006200*                                 PRIORITETSGRUPP                         
006300*                                 PRIORITY GROUP                          
006400     03 KDKVAANT             PIC 9.                                       
006500*                                 KOD ANTALSKONTR SKALL UTFÖRAS           
006600*                                 CODE THE QUANT WILL BE COUNTED          
006700     03 KDLAGEMB             PIC X(4).                                    
006800*                                 EMBALLAGEBETECKNING                     
006900*                                 PACKINGNOTATION                         
007000     03 KDRT                 PIC S9(3)           COMP-3.                  
007100*                                 REDOVISNINGSTYP                         
007200*                                 TYPE OF ACCOUNTING                      
007300     03 KDSORT               PIC X(2).                                    
007400*                                 SORT-KOD                                
007500*                                 UNIT OF MEASURE                         
007600     03 KVAVIS               PIC S9(7)           COMP-3.                  
007700*                                 AVISERAT ANTAL                          
007800*                                 QUANTITY NOTIFIED                       
007900     03 KVAVIS-KIT           PIC S9(7)           COMP-3.                  
008000*                                 AVISERAT ANTAL FÖR SATS                 
008100*                                 QUANTITY NOTIFEID FOR KIT               
008200     03 ADTRDEST-KIT         PIC X(3).                                    
008300*                                 TRANSPORTDESTINATION SATSER             
008400*                                 ADDRESS OF TRANSPORT KIT                
008500     03 KVAVIS-PRIO          PIC S9(7)           COMP-3.                  
008600*                                 BERÄKN PRIORITERAD KVANT TOT            
008700*                                 CALC PRIO QUANTITY TOT                  
008800     03 KVKVAPRIM-BER        PIC S9(7)           COMP-3.                  
008900*                                 BER ANTAL TILL PRIMÄRKONTROLL           
009000*                                 CALC QTY TO PRIMARY CONTROL             
009100     03 KVKVAPRIM-VER        PIC S9(7)           COMP-3.                  
009200*                                 VERKL ANTAL TILL PRIMÄRKONTROLL         
009300*                                 REAL QTY TO PRIMARY CONTROL             
009400     03 KVKVASEK-BER         PIC S9(7)           COMP-3.                  
009500*                                 BER. ANT TILL SEKUNDÄRKONTROLL          
009600*                                 CALC QTY TO SECONDARY CONTROL           
009700     03 KVKVASEK-VER         PIC S9(7)           COMP-3.                  
009800*                                 VERKL.ANT TILL SEKUNDÄRKONTROLL         
009900*                                 REAL QTY TO SECONDARY CONTROL           
010000     03 PRARTSTD             PIC S9(7)V9(2)      COMP-3.                  
010100*                                 ARTIKELSTANDARDPRIS                     
010200*                                 STANDARD PRICE                          
010300     03 ART-TIUPPDAT         PIC S9(7)           COMP-3.                  
010400*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
010500*                                 UPDATING DATE     (YYMMDD)              
010600     03 IDRADNR              PIC S9(5)           COMP-3.                  
010700*                                 RADNUMMER                               
010800*                                 LINE NO                                 
010900     03 ADINLOMR             PIC X(4).                                    
011000*                                 INLEVERANSOMRÅDE                        
011100*                                 RECEIVING AREA                          
011200     03 ADINLOMR-NXT         PIC X(4).                                    
011300*                                 INLEVERANSOMRÅDE NÄSTA                  
011400*                                 RECEIVING AREA NEXT                     
011500     03 FLDIVKLI             PIC X.                                       
011600*                                 DIVERSEKOLLIFLAGGA                      
011700*                                 VARIOUS CASE FLAG                       
011800     03 FLKVAANT             PIC X.                                       
011900*                                 ANTALSKONTROLL UTFÖRD                   
012000*                                 QUANTITY CONTROL DONE                   
012100     03 FLPRIO               PIC X.                                       
012200*                                 PRIORITERAD                             
012300*                                 HAS PRIORITY                            
012400     03 FLSATS               PIC X.                                       
012500*                                 SATSARTIKEL                             
012600*                                 KIT PART                                
012700     03 FLINLFB              PIC X.                                       
012800*                                 VALD TILL FÖRBEHANDLING                 
012900*                                 SELECTED FOR PRETREATEMENT              
013000     03 FLINLFP              PIC X.                                       
013100*                                 VALD TILL FÖRPACKNINGEN                 
013200*                                 SELECTED FOR PRE-PACKING                
013300     03 IDANSTNR             PIC S9(5)           COMP-3.                  
013400*                                 ANSTÄLLNINGSNUMMER                      
013500*                                 IDENTIFICATION NO EMPLOYEE              
013600     03 IDILIRAD             PIC S9(5)           COMP-3.                  
013700*                                 INLÄGGNINGSLISTERADNUMMER               
013800*                                 REPORTINGLISTLINENUMBER                 
013900     03 IDILIST              PIC 9(5).                                    
014000*                                 INLÄGGNINGSLISTEIDENTITET               
014100*                                 REPORTINGLIST-IDENTITY                  
014200     03 IDINLVGN             PIC 9(3).                                    
014300*                                 VAGNSIDENTITET                          
014400*                                 INTERNAL CARRIER ID                     
014500     03 RAD-IDLEVNR          PIC X(5).                                    
014600*                                 LEVERANTÖRNUMMER                        
014700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
014800     03 IDOKOLLI             PIC 9(9).                                    
014900*                                 ODETTE KOLLINUMMER                      
015000*                                 ODETTE CASE NUMBER                      
015100     03 RAD-KDINLPRIO        PIC S9(3)           COMP-3.                  
015200*                                 PRIORITETSGRUPP                         
015300*                                 PRIORITY GROUP                          
015400     03 KDINLSTA             PIC X(3).                                    
015500*                                 SYSTEMSTATUS INLEVERANS                 
015600*                                 SYSTEM STATUS RECEIVING                 
015700     03 KVINLART             PIC S9(7)           COMP-3.                  
015800*                                 ANTAL I PARTIRAD                        
015900*                                 QTY/LINE IN A LOT                       
016000     03 RAD-TIUPPDAT         PIC S9(7)           COMP-3.                  
016100*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
016200*                                 UPDATING DATE     (YYMMDD)              
016300     03 FLSPLPART            PIC X.                                       
016400*                                 AVISERAD INLEVERANS SPLITTAD PÅ         
016500*                                  MER ÄN ETT PARTI                       
016600*                                 ADVICED GOODS AMOUNT SPLITTED           
016700     03 ADTRDEST             PIC X(3).                                    
016800*                                 TRANSPORTDESTINATION                    
016900*                                 ADDRESS OF TRANSPORT                    
017000     03 FLANNULL             PIC X.                                       
017100*                                 ANNULLATION                             
017200*                                 CANCELLATION                            
017300*** END OF VILMAII-COPY LENGTH= 177 BYTES                                 
