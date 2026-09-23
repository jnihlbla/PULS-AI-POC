000100 01  KR-W4268601-CTX.                                                     
000200*                                 KONTROLLRAPPORTER FÖR                   
000300*                                 LISTNING                                
000400     03 KR-IDPTYP            PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 KR-W4268601-GRP.                                                  
000700*                                 KVALITET                                
000800*                                 KONTROLLRAPPORT                         
000900*                                 FÖR LISTNING I E+ PGM                   
001000        05 KR-IDKR           PIC 9(5).                                    
001100*                                 KONTROLLRAPPORT NUMMER                  
001200        05 KR-ADATTENT       PIC X(40).                                   
001300*                                 ATTENTIONADRESS                         
001400        05 KR-BEKRANS        PIC X(25).                                   
001500*                                 ANSVARIG                                
001600*                                                                         
001700        05 KR-BEKRBEH        PIC X(25).                                   
001800*                                 KONTROLLANT                             
001900        05 KR-BEKRPACK       PIC X(25).                                   
002000*                                 ANSVARIG FÖR PACKNING                   
002100*                                                                         
002200        05 KR-FLANNULL       PIC X.                                       
002300*                                 ANNULLATION                             
002400        05 KR-KDKRATG        PIC X.                                       
002500*                                 ÅTGÄRD BEGÄRD FÖR LEV.BER.FEL           
002600        05 KR-FLKRGODK       PIC X.                                       
002700*                                 GOKDKÄND                                
002800        05 KR-FLKRLFEL       PIC X.                                       
002900*                                 LEVERANTÖRSBEROENDE FEL                 
003000        05 KR-FLKROMK        PIC X.                                       
003100*                                 OMKOSTNADER KLAR FÖR DEB AV LEV         
003200        05 KR-IDANALYSNR     PIC S9(9)           COMP-3.                  
003300*                                 ANALYSNUMMER                            
003400        05 KR-IDARTNR        PIC S9(9)           COMP-3.                  
003500*                                 ARTIKELNUMMER                           
003600        05 KR-IDAVINR        PIC S9(7)           COMP-3.                  
003700*                                 AVI-NUMMER                              
003800        05 KR-IDFTG          PIC 9(2).                                    
003900*                                 FÖRETAGSID EKONOM REDOVISNING           
004000        05 KR-IDKONTO        PIC S9(11)          COMP-3.                  
004100*                                 KONTO                                   
004200        05 KR-IDKRATLF       PIC X(20).                                   
004300*                                 TELEFON TILL ANSVARIG                   
004400*                                                                         
004500        05 KR-IDKRFEL        OCCURS 3 TIMES                               
004600                             PIC X(2).                                    
004700*                                 FELKOD FÖR KONTROLLRAPPORT              
004800        05 KR-IDLEVG         PIC S9(5)           COMP-3.                  
004900*                                 LEVERANTÖRS GODSADRESS NUMMER           
005000        05 KR-IDLEVNR        PIC X(5).                                    
005100*                                 LEVERANTÖRNUMMER                        
005200        05 KR-IDLOPNRM       PIC S9(9)           COMP-3.                  
005300*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
005400*                                 (0VVDLLLLK)                             
005500        05 KR-IDSKYLT        PIC X(3).                                    
005600*                                 NATIONALITETSTECKEN                     
005700*                                 SPRÅKIDENTIFIKATION                     
005800        05 KR-IDDC           PIC X(2).                                    
005900*                                 IDENTIFIERARE LAGER                     
006000        05 KR-KDKRJUST       PIC X.                                       
006100*                                 JUSTERINGSKOD                           
006200        05 KR-KDKRSTA        PIC X.                                       
006300*                                 KONTROLLRAPPORT STATUS                  
006400        05 KR-KDKRUTF        PIC X.                                       
006500*                                 UTFÖRANDEKOD FÖR KONTROLLRAPP.          
006600        05 KR-KVANTMOT       PIC S9(7)           COMP-3.                  
006700*                                 ANTAL MOTTAGET                          
006800        05 KR-KVARBTID       PIC S9(2)V9(1)      COMP-3.                  
006900*                                 ANTAL MANTIMMAR                         
007000        05 KR-KVART-AAVV     PIC S9(7)           COMP-3.                  
007100*                                 ANTALSAVVIKELSE FÖR ARTIKEL             
007200        05 KR-KVART-BEH      PIC S9(7)           COMP-3.                  
007300*                                 ANTAL ARTIKLAR SOM BEHÅLLES             
007400        05 KR-KVART-EJ-GODK  PIC S9(7)           COMP-3.                  
007500*                                 ANTAL EJ GODKÄNDA ARTIKLAR              
007600        05 KR-KVART-KJUST    PIC S9(7)           COMP-3.                  
007700*                                 ANTAL ARTIKLAR KVAL.JUSTERAS            
007800        05 KR-KVART-KONTR    PIC S9(7)           COMP-3.                  
007900*                                 ANTAL KONTROLLERAD ARTIKLAR             
008000        05 KR-KVART-RET      PIC S9(7)           COMP-3.                  
008100*                                 ANTAL ARTIKLAR I RETUR                  
008200        05 KR-KVART-SJUST    PIC S9(7)           COMP-3.                  
008300*                                 ANTAL SALDOJUSTERADE ARTIKLAR           
008400        05 KR-KVART-SKROT    PIC S9(7)           COMP-3.                  
008500*                                 ANTAL SKROTADE ARTIKLAR                 
008600        05 KR-KVAVIS         PIC S9(7)           COMP-3.                  
008700*                                 AVISERAT ANTAL                          
008800        05 KR-KVKRBEH        PIC 9(2)V9(1).                               
008900*                                 BEHANDLINGSTID FÖR KR                   
009000        05 KR-KVKRKNTR       PIC S9              COMP-3.                  
009100*                                 REKNEVERK ANTAL/KVALITET AVV            
009200        05 KR-KVKRPACK       PIC 9(2)V9(1).                               
009300*                                 PACKNINGSTID                            
009400        05 KR-SUMAT          PIC S9(7)V9(2)      COMP-3.                  
009500*                                 MATERIALKOSTNAD                         
009600        05 KR-SUOMK          PIC S9(7)           COMP-3.                  
009700*                                 SUMMA OMKOSTNADER                       
009800        05 KR-TEKRFEL-002    OCCURS 2 TIMES                               
009900                             PIC X(70).                                   
010000*                                 FELBESKRIVNING I FRI TEXT               
010100        05 KR-TEKRPLT        PIC X(20).                                   
010200*                                 GODS PLACERAT                           
010300        05 KR-TEKRSPEC-ATID  PIC X(30).                                   
010400*                                 SPECIFIKATION ARBETSTID                 
010500        05 KR-TEKRSPEC-MAT   PIC X(30).                                   
010600*                                 SPECIFIKATION MATERIALKOSTNAD           
010700        05 KR-TEKRSPEC-OMK   PIC X(30).                                   
010800*                                 SPECIFIKATION OMKOSTNADER               
010900        05 KR-TIAVSDAT       PIC S9(7)           COMP-3.                  
011000*                                 AVISERINGSDATUM (YYMMDD)                
011100        05 KR-TIKRANS        PIC S9(7)           COMP-3.                  
011200*                                 DATUM KONTROLLRAPPORT GODKÄND           
011300        05 KR-TIKRPACK       PIC S9(7)           COMP-3.                  
011400*                                 PACKNINGSDATUM                          
011500        05 KR-TIREGDAT       PIC S9(7)           COMP-3.                  
011600*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
011700*** END OF VILMAII-COPY LENGTH= 521 BYTES                                 
