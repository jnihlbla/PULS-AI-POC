000100 01  ART-W6D111.                                                          
000200*                                 INLEVERANSREGISTER                      
000300*                                 PARTI SEGMENT                           
000400*                                 FYSISK NYCKEL: IDRADNR-INL              
000500*                                 SÖKBEGREPP:    IDARTNR                  
000600     03 ART-IDRADNR-INL      PIC S9(5)           COMP-3.                  
000700*                                 RADNUMMER INLEVERANS                    
000800*                                 LINE NUMBER GOODS RECEIVING             
000900     03 ART-IDARTNR          PIC S9(9)           COMP-3.                  
001000*                                 ARTIKELNUMMER                           
001100*                                 PART NUMBER                             
001200     03 ART-TIUPPDAT         PIC S9(7)           COMP-3.                  
001300*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
001400*                                 UPDATING DATE     (YYMMDD)              
001500     03 ART-ADGANG           PIC S9(3)           COMP-3.                  
001600*                                 GÅNG                                    
001700*                                 AISLE                                   
001800     03 ART-ADLAGOMR         PIC S9(3)           COMP-3.                  
001900*                                 LAGEROMRÅDE                             
002000*                                 AREA                                    
002100     03 ART-ADPLATS          PIC S9(5)           COMP-3.                  
002200*                                 LAGERPLATSNUMMER                        
002300*                                 LOCATION                                
002400     03 ART-ADTRDEST-KIT     PIC X(3).                                    
002500*                                 TRANSPORTDESTINATION SATSER             
002600*                                 ADDRESS OF TRANSPORT KIT                
002700     03 ART-BEART            PIC X(25).                                   
002800*                                 ARTIKELBENÄMNING                        
002900*                                 PART DESCRIPTION                        
003000     03 ART-BEFT             PIC S9(3)           COMP-3.                  
003100*                                 FÖRPACKNINGSTYP                         
003200*                                 PACKAGING TYPE                          
003300     03 ART-FLETIKETT        PIC X.                                       
003400*                                 ANGER ATT ETIKETT ÄR PRINTAD            
003500*                                 PRE-PACKING LABEL ARE PRINTED           
003600     03 ART-FLFEL            PIC X.                                       
003700*                                 ALLMÄN FELFLAGGA                        
003800*                                 GENERAL ERROR FLAG                      
003900     03 ART-FLKLAR           PIC X.                                       
004000*                                 AVSLUTNINGSMARKERING                    
004100*                                 FINISHED FLAG                           
004200     03 ART-FLKVAFEL         PIC X.                                       
004300*                                 FLAGGA KVALITETSFEL                     
004400*                                 FLAG QUALITY ERROR                      
004500     03 ART-FLKVAKAR         PIC X.                                       
004600*                                 FLAGGA KARANTÄN                         
004700*                                 FLAG QUARANTINE                         
004800     03 ART-IDFKNGRP         PIC S9(5)           COMP-3.                  
004900*                                 FUNKTIONSGRUPP                          
005000*                                 FUNCTION GROUP                          
005100     03 ART-IDLOPNRM         PIC S9(9)           COMP-3.                  
005200*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
005300*                                 (0VVDLLLLK)                             
005400*                                 SERIAL NO RECEIVING REPORT              
005500*                                 (0WWDLLLLC)                             
005600     03 ART-IDDC             PIC X(2).                                    
005700*                                 IDENTIFIERARE LAGER                     
005800*                                 WAREHOUSE IDENTIFIER                    
005900     03 ART-KDARTURS         PIC X(2).                                    
006000*                                 ARTIKELURSPRUNGSKOD                     
006100*                                 COUNTRY OF ORIGIN                       
006200     03 ART-KDFARLIG         PIC S9              COMP-3.                  
006300*                                 KOD FÖR FARLIGT GODS                    
006400*                                 DANGEROUS GOODS CODE                    
006500     03 ART-KDINLPRIO        PIC S9(3)           COMP-3.                  
006600*                                 PRIORITETSGRUPP                         
006700*                                 PRIORITY GROUP                          
006800     03 ART-KDKVAANT         PIC 9.                                       
006900*                                 KOD ANTALSKONTR SKALL UTFÖRAS           
007000*                                 CODE THE QUANT WILL BE COUNTED          
007100     03 ART-KDLAGEMB         PIC X(4).                                    
007200*                                 EMBALLAGEBETECKNING                     
007300*                                 PACKINGNOTATION                         
007400     03 ART-KDRT             PIC S9(3)           COMP-3.                  
007500*                                 REDOVISNINGSTYP                         
007600*                                 TYPE OF ACCOUNTING                      
007700     03 ART-KDSORT           PIC X(2).                                    
007800*                                 SORT-KOD                                
007900*                                 UNIT OF MEASURE                         
008000     03 ART-KVAVIS           PIC S9(7)           COMP-3.                  
008100*                                 AVISERAT ANTAL                          
008200*                                 QUANTITY NOTIFIED                       
008300     03 ART-KVAVIS-KIT       PIC S9(7)           COMP-3.                  
008400*                                 AVISERAT ANTAL FÖR SATS                 
008500*                                 QUANTITY NOTIFEID FOR KIT               
008600     03 ART-KVAVIS-PRIO      PIC S9(7)           COMP-3.                  
008700*                                 BERÄKN PRIORITERAD KVANT TOT            
008800*                                 CALC PRIO QUANTITY TOT                  
008900     03 ART-KVKVAPRIM-BER    PIC S9(7)           COMP-3.                  
009000*                                 BER ANTAL TILL PRIMÄRKONTROLL           
009100*                                 CALC QTY TO PRIMARY CONTROL             
009200     03 ART-KVKVAPRIM-VER    PIC S9(7)           COMP-3.                  
009300*                                 VERKL ANTAL TILL PRIMÄRKONTROLL         
009400*                                 REAL QTY TO PRIMARY CONTROL             
009500     03 ART-KVKVASEK-BER     PIC S9(7)           COMP-3.                  
009600*                                 BER. ANT TILL SEKUNDÄRKONTROLL          
009700*                                 CALC QTY TO SECONDARY CONTROL           
009800     03 ART-KVKVASEK-VER     PIC S9(7)           COMP-3.                  
009900*                                 VERKL.ANT TILL SEKUNDÄRKONTROLL         
010000*                                 REAL QTY TO SECONDARY CONTROL           
010100     03 ART-KVMP             PIC S9(7)           COMP-3.                  
010200*                                 MAXPUNKT                                
010300*                                 MAXIMUM POINT                           
010400     03 ART-PRARTSTD         PIC S9(7)V9(2)      COMP-3.                  
010500*                                 ARTIKELSTANDARDPRIS                     
010600*                                 STANDARD PRICE                          
010700     03 ART-VKART            PIC S9(7)           COMP-3.                  
010800*                                 ARTIKELVIKT (G)                         
010900*                                 PART WEIGHT (G)                         
011000     03 ART-VLARTNTO         PIC S9(8)V9(1)      COMP-3.                  
011100*                                 ARTIKELVOLYM NETTO (CM3)                
011200*                                 PART NET VOLUME    (CM3)                
011300     03 ART-FLANNULL         PIC X.                                       
011400*                                 ANNULLATION                             
011500*                                 CANCELLATION                            
011600     03 ART-FLSPLPART        PIC X.                                       
011700*                                 AVISERAD INLEVERANS SPLITTAD PÅ         
011800*                                  MER ÄN ETT PARTI                       
011900*                                 ADVICED GOODS AMOUNT SPLITTED           
012000     03 ART-ADTRDEST         PIC X(3).                                    
012100*                                 TRANSPORTDESTINATION                    
012200*                                 ADDRESS OF TRANSPORT                    
012300     03 ART-KDKVAINL         PIC X(2).                                    
012400*                                 INLEVERANS TILL KVALITETSKOLL           
012500*                                 INBOUND FOR QUALITY CHECK               
012600     03 ART-FILLER           PIC X(6).                                    
012700*** END OF VILMAII-COPY LENGTH= 137 BYTES                                 
