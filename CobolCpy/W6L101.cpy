000100 01  UPPF-W6L101.                                                         
000200*                                 UPPFÖLJINGNSREGISTER                    
000300*                                 KVALITET                                
000400*                                 FYSISK NYCKEL: IDLOPNRM                 
000500     03 UPPF-IDLOPNRM        PIC S9(9)           COMP-3.                  
000600*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000700*                                 (0VVDLLLLK)                             
000800*                                 SERIAL NO RECEIVING REPORT              
000900*                                 (0WWDLLLLC)                             
001000     03 UPPF-ADKVAULG        PIC X(2).                                    
001100*                                 PLATS UNDERLAG KVAL.KONTROLL            
001200*                                 LOCATION  QUALITY DOCUMENTATION         
001300     03 UPPF-BEANST          PIC X(25).                                   
001400*                                 ANSTÄLLDS NAMN                          
001500*                                 NAME OF EMPLOYED                        
001600     03 UPPF-FLANNULL        PIC X.                                       
001700*                                 ANNULLATION                             
001800*                                 CANCELLATION                            
001900     03 UPPF-FLKVARED        PIC X.                                       
002000*                                 REDUCERAD KONTROLL FLAGGA               
002100*                                 REDUCED CONTROL FLAG                    
002200     03 UPPF-FLKVAUTV-ANT    PIC X.                                       
002300*                                 PARTIET UTVALT FÖR ANTALSKONTR          
002400*                                 CONSIGNMENT CHOSEN QTY INSP             
002500     03 UPPF-FLKVAUTV-KVAL   PIC X.                                       
002600*                                 PARTIET UTVALT KVALITETSKONTR           
002700*                                 CONSIGNMENT CHOSEN QUAL.INSP.           
002800     03 UPPF-FLSKPSAK        PIC X.                                       
002900*                                 FLAGGA SKIPLOT-SÄKRAD                   
003000*                                 FLAG SKIPLOT ASSURED                    
003100     03 UPPF-IDARTNR         PIC S9(9)           COMP-3.                  
003200*                                 ARTIKELNUMMER                           
003300*                                 PART NUMBER                             
003400     03 UPPF-IDDC            PIC X(2).                                    
003500*                                 IDENTIFIERARE LAGER                     
003600*                                 WAREHOUSE IDENTIFIER                    
003700     03 UPPF-IDLEVNR         PIC X(5).                                    
003800*                                 LEVERANTÖRNUMMER                        
003900*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
004000     03 UPPF-IDUSER-ADM      PIC X(8).                                    
004100*                                 ANVÄNDAR-ID ADMINISTRATIV KONTR         
004200*                                 USER ID ADMINISTRATIVE INSPEC.          
004300     03 UPPF-IDUSER-PRI      PIC X(8).                                    
004400*                                 ANVÄNDAR-ID PRIMÄRKONTROLL              
004500*                                 USER ID PRIMARY INSPECTION              
004600     03 UPPF-IDUSER-SEK      PIC X(8).                                    
004700*                                 ANVÄNDAR-ID SEKUNDÄRKONTROLL            
004800*                                 USER ID SECONDARY INSPECTION            
004900     03 UPPF-KDKVAKTL.                                                    
005000*                                 KVALITETSKONTROLL KOD                   
005100*                                 QUALITY INSPECTION CODE                 
005200        05 UPPF-KDKVATYP     PIC X.                                       
005300*                                 NORMAL/VERIFIKATIONS KVAL.KONTR         
005400*                                 NORMAL/VERIFICATION QUAL.INSP.          
005500        05 UPPF-IDPROVPL-PRI PIC X.                                       
005600*                                 PROVTAGNINGSPLAN PRIM.KONTROLL          
005700*                                 SAMPLE PLAN PRIMARY INSPECTION          
005800        05 UPPF-IDPROVPL-SEK PIC X.                                       
005900*                                 PROVTAGNINGSPLAN SEK.KONTROLL           
006000*                                 SAMPLE PLAN SEC. INSPECTION             
006100        05 UPPF-KDKVAULG     PIC X.                                       
006200*                                 UNDERLAG FÖR KVALITETSKONTROLL          
006300*                                 DOCUMENTATION FOR QUAL.INSP.            
006400     03 UPPF-KDKVASTA-ADM    PIC X.                                       
006500*                                 STATUS ADMINISTRATIV KONTROLL           
006600*                                 STATUS ADMINISTRATIVE                   
006700*                                 INSPECTION                              
006800     03 UPPF-KDKVASTA-ANT    PIC X.                                       
006900*                                 STATUS ANTALSKONTROLL                   
007000*                                 STATUS QUANTITY INSPECTION              
007100     03 UPPF-KDKVASTA-PRI    PIC X.                                       
007200*                                 STATUS PRIMÄRKONTROLL                   
007300*                                 STATUS PRIMARY INSPECTION               
007400     03 UPPF-KDKVASTA-SEK    PIC X.                                       
007500*                                 STATUS SEKUNDÄRKONTROLL                 
007600*                                 STATUS SECONDARY INSPECTION             
007700*                                                                         
007800     03 UPPF-KVKVAPRIM       PIC S9(7)           COMP-3.                  
007900*                                 ANTAL TILL PRIMÄRKONTROLL               
008000*                                 QTY TO PRIMARY CONTROL                  
008100     03 UPPF-KVKVASEK        PIC S9(7)           COMP-3.                  
008200*                                  ANTAL TILL SEKUNDÄRKONTROLL            
008300*                                  QTY TO SECONDARY CONTROL               
008400     03 UPPF-TIREGDAT        PIC S9(7)           COMP-3.                  
008500*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
008600*                                 REGISTRATION DATE (YYMMDD)              
008700     03 UPPF-FILLER          PIC X(7).                                    
008800*** END OF VILMAII-COPY LENGTH= 100 BYTES                                 
