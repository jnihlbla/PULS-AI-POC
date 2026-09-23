000100 01  W42680.                                                              
000200*                                 UPPFÖLJINGSFIL                          
000300*                                 KVALITET                                
000400     03 IDLOPNRM             PIC S9(9)           COMP-3.                  
000500*                                 LÖPNUMMER MOTTAGNINGSRAPPORT            
000600*                                 (0VVDLLLLK)                             
000700*                                 SERIAL NO RECEIVING REPORT              
000800*                                 (0WWDLLLLC)                             
000900     03 ADKVAULG             PIC X(2).                                    
001000*                                 PLATS UNDERLAG KVAL.KONTROLL            
001100*                                 LOCATION  QUALITY DOCUMENTATION         
001200     03 BEANST               PIC X(25).                                   
001300*                                 ANSTÄLLDS NAMN                          
001400*                                 NAME OF EMPLOYED                        
001500     03 FLANNULL             PIC X.                                       
001600*                                 ANNULLATION                             
001700*                                 CANCELLATION                            
001800     03 FLKVARED             PIC X.                                       
001900*                                 REDUCERAD KONTROLL FLAGGA               
002000*                                 REDUCED CONTROL FLAG                    
002100     03 FLKVAUTV-ANT         PIC X.                                       
002200*                                 PARTIET UTVALT FÖR ANTALSKONTR          
002300*                                 CONSIGNMENT CHOSEN QTY INSP             
002400     03 FLKVAUTV-KVAL        PIC X.                                       
002500*                                 PARTIET UTVALT KVALITETSKONTR           
002600*                                 CONSIGNMENT CHOSEN QUAL.INSP.           
002700     03 FLSKPSAK             PIC X.                                       
002800*                                 FLAGGA SKIPLOT-SÄKRAD                   
002900*                                 FLAG SKIPLOT ASSURED                    
003000     03 IDARTNR              PIC S9(9)           COMP-3.                  
003100*                                 ARTIKELNUMMER                           
003200*                                 PART NUMBER                             
003300     03 IDDC                 PIC X(2).                                    
003400*                                 IDENTIFIERARE LAGER                     
003500*                                 WAREHOUSE IDENTIFIER                    
003600     03 IDLEVNR              PIC X(5).                                    
003700*                                 LEVERANTÖRNUMMER                        
003800*                                 SUPPLIER NUMBER (VENDOR NUMBER)         
003900     03 IDUSER-ADM           PIC X(8).                                    
004000*                                 ANVÄNDAR-ID ADMINISTRATIV KONTR         
004100*                                 USER ID ADMINISTRATIVE INSPEC.          
004200     03 IDUSER-PRI           PIC X(8).                                    
004300*                                 ANVÄNDAR-ID PRIMÄRKONTROLL              
004400*                                 USER ID PRIMARY INSPECTION              
004500     03 IDUSER-SEK           PIC X(8).                                    
004600*                                 ANVÄNDAR-ID SEKUNDÄRKONTROLL            
004700*                                 USER ID SECONDARY INSPECTION            
004800     03 KDKVAKTL.                                                         
004900*                                 KVALITETSKONTROLL KOD                   
005000*                                 QUALITY INSPECTION CODE                 
005100        05 KDKVATYP          PIC X.                                       
005200*                                 NORMAL/VERIFIKATIONS KVAL.KONTR         
005300*                                 NORMAL/VERIFICATION QUAL.INSP.          
005400        05 IDPROVPL-PRI      PIC X.                                       
005500*                                 PROVTAGNINGSPLAN PRIM.KONTROLL          
005600*                                 SAMPLE PLAN PRIMARY INSPECTION          
005700        05 IDPROVPL-SEK      PIC X.                                       
005800*                                 PROVTAGNINGSPLAN SEK.KONTROLL           
005900*                                 SAMPLE PLAN SEC. INSPECTION             
006000        05 KDKVAULG          PIC X.                                       
006100*                                 UNDERLAG FÖR KVALITETSKONTROLL          
006200*                                 DOCUMENTATION FOR QUAL.INSP.            
006300     03 KDKVASTA-ADM         PIC X.                                       
006400*                                 STATUS ADMINISTRATIV KONTROLL           
006500*                                 STATUS ADMINISTRATIVE                   
006600*                                 INSPECTION                              
006700     03 KDKVASTA-ANT         PIC X.                                       
006800*                                 STATUS ANTALSKONTROLL                   
006900*                                 STATUS QUANTITY INSPECTION              
007000     03 KDKVASTA-PRI         PIC X.                                       
007100*                                 STATUS PRIMÄRKONTROLL                   
007200*                                 STATUS PRIMARY INSPECTION               
007300     03 KDKVASTA-SEK         PIC X.                                       
007400*                                 STATUS SEKUNDÄRKONTROLL                 
007500*                                 STATUS SECONDARY INSPECTION             
007600*                                                                         
007700     03 KVKVAPRIM            PIC S9(7)           COMP-3.                  
007800*                                 ANTAL TILL PRIMÄRKONTROLL               
007900*                                 QTY TO PRIMARY CONTROL                  
008000     03 KVKVASEK             PIC S9(7)           COMP-3.                  
008100*                                  ANTAL TILL SEKUNDÄRKONTROLL            
008200*                                  QTY TO SECONDARY CONTROL               
008300     03 TIREGDAT             PIC S9(7)           COMP-3.                  
008400*                                 REGISTRERINGSDATUM (ÅÅMMDD)             
008500*                                 REGISTRATION DATE (YYMMDD)              
008600*** END OF VILMAII-COPY LENGTH= 93 BYTES                                  
