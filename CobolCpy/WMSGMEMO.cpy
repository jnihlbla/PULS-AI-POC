000010*** EDIT ALLOWED                                                          
000100 01  MEMO-WMSGMEMO.                                                       
000200*                                                                         
000300*    COPYTEXT FÖR SÄNDNING AV MEMO FRÅN TP-PROGRAM                        
000400*                                                                         
000500*    FÖR ATT SÄNDA EN TEXT FRÅN ETT TP-PROGRAM TILL EN                    
000600*    SPECIELL MEMOBREVLÅDA SÄNDS NEDANSTÅENDE COPYTEXT                    
000700*    IFYLLD TILL TRANSAKTION W0T541X.                                     
000800*                                                                         
000900*    LÄNGDEN SKALL VARA 4999                                              
001000*    DISTRIBUTIONSGRUPP OCH MEMOID SKALL VARA VÄNSTER-                    
001100*    JUSTERADE OCH BLANKUTFYLLDA TILL HÖGER.                              
001200*    TRANSAKTIONSKODEN SKALL VARA W0T541X                                 
001300*    FÄLTET FÖR ANTAL RADER SKALL ANGE HUR MÅNGA RADER                    
001400*    SOM SKALL SÄNDAS TILL MEMO. GODKÄNDA VÄRDEN ÄR                       
001500*    MELLAN 1 OCH 75. OM VÄRDET ÄR UTANFÖR DESSA GRÄNSER                  
001600*    KOMMER ETT FELMEDDELANDE PÅ SISTA RADEN I MEMOT.                     
001700*                                                                         
001800*    OM DET ÄR NÅGOT PROBLEM MED ÖVERFÖRINGEN TILL MEMO KOMMER            
001900*    TRANSAKTION W0T541X ATT GÖRA ABEND.                                  
002000*                                                                         
002100*    SÄNDNING AV MEMO GÖRS MOT ETT ALT-PCB SOM HAR TRANSAKTION            
002200*    W0T541X ANGIVET VID GENERERINGEN.                                    
002300*                                                                         
002400*    CALL-EXEMPEL.                                                        
002500*         CALL CBLTDLI USING ISRT ALT-PCB MEMO-WMSGMEMO                   
002600*                                                                         
002610*                                                                         
002620*                                                                         
002700     03 MEMO-KVLL            PIC S9(4) VALUE +4999 COMP SYNC.             
002800*                                * LÄNGD AV MEDDELANDET                   
002900*                                * SKALL VARA +4999                       
003000     03 MEMO-KDZ1            PIC X     VALUE LOW-VALUE.                   
003100*                                * FLAGGOR = LOW-VALUE                    
003200     03 MEMO-KDZ2            PIC X     VALUE LOW-VALUE.                   
003300*                                * FLAGGOR = LOW-VALUE                    
003400     03 MEMO-KDTRANS         PIC X(8)  VALUE 'W0T541X '.                  
003500*                                * TRANSKOD 'W0T541X '                    
003600     03 MEMO-IDTRANS         PIC X(4).                                    
003700*                                * ANGER FRÅN VILKEN MID                  
003800*                                * MEDDELANDET KOMMER                     
003900     03 MEMO-KDMFSFOR        PIC X(1).                                    
004000*                                * MFS-FORMAT                             
004100*                                * 1 SVENSKA LEDTEXTER                    
004200*                                * 2 ENGELSKA LEDTEXTER                   
004300     03 MEMO-IDMEMODG        PIC X(8).                                    
004400*                                * DG FÖR MOTTAGARE                       
004500     03 MEMO-IDMEMO          PIC X(8).                                    
004600*                                * MEMOID FÖR MOTTAGARE                   
004700     03 MEMO-IDMTITEL        PIC X(14).                                   
004800*                                * MEMO TITEL                             
004900     03 MEMO-KVMEMRAD        PIC 9(2).                                    
005000*                                * ANTAL TEXTRADER                        
005100     03 MEMO-TEMEMO          PIC X(66) OCCURS 75.                         
005200*                                * MEMO TEXT RAD                          
005300*** END COPY WMSGMEMOC0  LENGTH=4999  OLD LENGTH=4999                     
