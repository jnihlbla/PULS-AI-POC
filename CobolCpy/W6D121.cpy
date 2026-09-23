000100 01  RAD-W6D121.                                                          
000200*                                 INLEVERANSREGISTER                      
000300*                                 RAD SEGMENT                             
000400*                                 FYSISK NYCKEL: IDRADNR                  
000500     03 RAD-IDRADNR          PIC S9(5)           COMP-3.                  
000600*                                 RADNUMMER                               
000700*                                 LINE NO                                 
000800     03 RAD-ADINLOMR         PIC X(4).                                    
000900*                                 INLEVERANSOMRÅDE                        
001000*                                 RECEIVING AREA                          
001100     03 RAD-ADINLOMR-NXT     PIC X(4).                                    
001200*                                 INLEVERANSOMRÅDE NÄSTA                  
001300*                                 RECEIVING AREA NEXT                     
001400     03 RAD-FLDIVKLI         PIC X.                                       
001500*                                 DIVERSEKOLLIFLAGGA                      
001600*                                 VARIOUS CASE FLAG                       
001700     03 RAD-FLINLFP          PIC X.                                       
001800*                                 VALD TILL FÖRPACKNINGEN                 
001900*                                 SELECTED FOR PRE-PACKING                
002000     03 RAD-FLKVAANT         PIC X.                                       
002100*                                 ANTALSKONTROLL UTFÖRD                   
002200*                                 QUANTITY CONTROL DONE                   
002300     03 RAD-FLSVSLS          PIC X.                                       
002400*                                 ANGER ATT PARTIRADENS KVANTITET         
002500*                                  SKALL UPPDATERA SVS-LAGERSALDO         
002600     03 RAD-FLPRIO           PIC X.                                       
002700*                                 PRIORITERAD                             
002800*                                 HAS PRIORITY                            
002900     03 RAD-FLSATS           PIC X.                                       
003000*                                 SATSARTIKEL                             
003100*                                 KIT PART                                
003200     03 RAD-FLINLFB          PIC X.                                       
003300*                                 VALD TILL FÖRBEHANDLING                 
003400*                                 SELECTED FOR PRETREATEMENT              
003500     03 RAD-IDANSTNR         PIC S9(5)           COMP-3.                  
003600*                                 ANSTÄLLNINGSNUMMER                      
003700*                                 IDENTIFICATION NO EMPLOYEE              
003800     03 RAD-IDILIRAD         PIC S9(5)           COMP-3.                  
003900*                                 INLÄGGNINGSLISTERADNUMMER               
004000*                                 REPORTINGLISTLINENUMBER                 
004100     03 RAD-IDILIST          PIC 9(5).                                    
004200*                                 INLÄGGNINGSLISTEIDENTITET               
004300*                                 REPORTINGLIST-IDENTITY                  
004400     03 RAD-IDINLVGN         PIC 9(3).                                    
004500*                                 VAGNSIDENTITET                          
004600*                                 INTERNAL CARRIER ID                     
004700     03 RAD-IDLEVNR-KOLLI    PIC X(5).                                    
004800*                                 LEVERANTÖRNUMMER KOLLI                  
004900*                                 SUPPLIER NUMBER CASE                    
005000     03 RAD-IDOKOLLI         PIC 9(9).                                    
005100*                                 ODETTE KOLLINUMMER                      
005200*                                 ODETTE CASE NUMBER                      
005300     03 RAD-KDINLPRIO        PIC S9(3)           COMP-3.                  
005400*                                 PRIORITETSGRUPP                         
005500*                                 PRIORITY GROUP                          
005600     03 RAD-KDINLSTA         PIC X(3).                                    
005700*                                 SYSTEMSTATUS INLEVERANS                 
005800*                                 SYSTEM STATUS RECEIVING                 
005900     03 RAD-KVINLART         PIC S9(7)           COMP-3.                  
006000*                                 ANTAL I PARTIRAD                        
006100*                                 QTY/LINE IN A LOT                       
006200     03 RAD-TIUPPDAT         PIC S9(7)           COMP-3.                  
006300*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
006400*                                 UPDATING DATE     (YYMMDD)              
006500     03 RAD-FILLER           PIC X(6).                                    
006600*** END OF VILMAII-COPY LENGTH= 65 BYTES                                  
