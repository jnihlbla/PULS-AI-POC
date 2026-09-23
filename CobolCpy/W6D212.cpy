000100 01  LEV-W6D212.                                                          
000200*                                 KVALITETSKONTROLL                       
000300*                                 LEVERANTÖRS SEGMENT                     
000400*                                 FYSISK NYCKEL: IDLEVNR                  
000500     03 LEV-IDLEVNR          PIC X(5).                                    
000600*                                 LEVERANTÖRNUMMER                        
000700*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
000800     03 LEV-FLKVARED         PIC X.                                       
000900*                                 REDUCERAD KONTROLL FLAGGA               
001000*                                 REDUCED CONTROL FLAG                    
001100     03 LEV-FLKVASAK         PIC X.                                       
001200*                                 FLAGGA KVALITETSSÄKRAD                  
001300*                                 FLAG QUALITY ASSURED                    
001400     03 LEV-FLSKPSAK         PIC X.                                       
001500*                                 FLAGGA SKIPLOT-SÄKRAD                   
001600*                                 FLAG SKIPLOT ASSURED                    
001700     03 LEV-FLUPG            PIC X.                                       
001800*                                 FLAGGA UTFALLSPROV GODKÄNT              
001900*                                 QUALITY TEST ACCEPTED FLAG              
002000     03 LEV-KDKVASAK         PIC X.                                       
002100*                                 ANSVARIG FÖR KVALITETSSÄKRING           
002200*                                 RESPONSIBLE FOR QUAL.ASSURANCE          
002300     03 LEV-KDKVAUP          PIC X.                                       
002400*                                 ANSVARIG FÖR UTFALLSPROV                
002500*                                 RESP. FOR INITIAL SAMPLE TEST           
002600     03 LEV-KVSKPLOT-PRI     PIC S9(3)           COMP-3.                  
002700*                                 SKIPLOT RÄKNARE PRIMÄR KONTROLL         
002800*                                 SKIPLOT COUNTER PRIMARY INSP.           
002900     03 LEV-KVSKPLOT-SEK     PIC S9(3)           COMP-3.                  
003000*                                 SKIPLOT RÄKNARE SEK. KONTROLL           
003100*                                 SKIPLOT COUNTER SEC. INSPECTION         
003200     03 LEV-TIKVASAK         PIC S9(7)           COMP-3.                  
003300*                                 DATUM KVALITETSSÄKRAD                   
003400*                                 DATE QUALITY ASSURED                    
003500     03 LEV-TIUPG            PIC S9(7)           COMP-3.                  
003600*                                 TID NÄR UTFALLSPROV GJORTS              
003700*                                 DATE FOR ENDED QUALITY CONTROL          
003800     03 LEV-TIUPPDAT         PIC S9(7)           COMP-3.                  
003900*                                 UPPDATERINGSDATUM  (ÅÅMMDD)             
004000*                                 UPDATING DATE     (YYMMDD)              
004100*** END OF VILMAII-COPY LENGTH= 27 BYTES                                  
