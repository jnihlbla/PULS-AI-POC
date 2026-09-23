000100 01  W11636-CTX.                                                          
000200*                                 ERSÄTTNINGSMEDDELANDEN USA/CAN          
000300*                                                                         
000400     03 IDPTYP               PIC X(3).                                    
000500*                                 POSTTYP                                 
000600     03 IDARTNR20            PIC X(20).                                   
000700*                                 20-STÄLLIGT ARTIKELNUMMER FÖR A         
000800*                                 S400 (VIPS)                             
000900*                                 FORMATET ÄR HÖGERJUSTERAT MED I         
001000*                                 NLEDANDE                                
001100*                                 BLANKTECKEN, OCH UTAN INLEDANDE         
001200*                                  NOLLOR.                                
001300     03 TIERSDAT-002         PIC 9(6).                                    
001400*                                 (ÅÅMMDD)           TIERSDAT-002         
001500*                                 DEF. DATUM FÖR ERSÄTTNING               
001600     03 FLDEL                PIC X.                                       
001700*                                 ALLMÄN FLAGGA                           
001800     03 KDERS                PIC 9(3).                                    
001900*                                 ERSÄTTNINGSKOD                          
002000     03 IDKORTNR             PIC 9(3).                                    
002100*                                 KORTNUMMER                              
002200*                                 (RADLÖPNR FÖR ERSÄTTNINGSINFO)          
002210     03 KDTEXTGR             PIC 9(2).                                    
002300     03 FLTEXT               PIC X.                                       
002400*                                 FINNS TEXTINFORMATION ?                 
002500     03 IDARTNR20-TILLK      PIC X(20).                                   
002600*                                 20-STÄLLIGT ARTIKELNUMMER FÖR A         
002700*                                 S400 (VIPS)                             
002800*                                 FORMATET ÄR HÖGERJUSTERAT MED I         
002900*                                 NLEDANDE                                
003000*                                 BLANKTECKEN, OCH UTAN INLEDANDE         
003100*                                  NOLLOR.                                
003200     03 DIERS-TILLK          PIC 9(4)V9(3).                               
003300*                                 TILLKOMMANDE ARTIKELANTAL               
003400     03 KDARTUTG             PIC 9.                                       
003401     03 KDUTGSTA             PIC X.                                       
003402     03 KDUTGSTR             PIC X.                                       
003410     03 KDPRODSL             PIC 9(2).                                    
003500*                                 PRODUKTSLAG                             
003600*** END OF VILMAII-COPY LENGTH= 71 BYTES                                  
