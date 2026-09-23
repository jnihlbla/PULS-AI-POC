//W33092OK JOB (540W3300100W33092OK,W100),'RTN W330B6',                         
//         CLASS=K                                                              
/*JOBPARM TIME=60,LINES=30,CARDS=0,FORMS=1800,LINECT=00                         
//PROC  JCLLIB ORDER=(W.QASE.PROCLIB)                                           
//ENV  INCLUDE MEMBER=ENVQASE                                                   
/*ROUTE XEQ LOCAL                                                               
/*ROUTE PRINT LOCAL                                                             
/*AFTER MEMOAPIX                                                                
//*---  WMEMOSND,EXC                                                            
//*                                                                             
//MEMOSND EXEC WMEMOSND,DSIN=NULLFILE                                           
)SEND                                                                           
*       SÄNDER MEMO TILL SYSTEMAVD.                                             
  TITLE WIN.XBMS.01                                                             
  OPTION FORCE                                                                  
  DEST  ELEONOR.OSTROM(A)VOLVO.COM                                              
  MEMO                                                                          
  ARTIKELSTATISTIK    ARTIKELSTATISTIK    ARTIKELSTATISTIK                      
  ARTIKELSTATISTIK    ARTIKELSTATISTIK    ARTIKELSTATISTIK                      
  ARTIKELSTATISTIK    ARTIKELSTATISTIK    ARTIKELSTATISTIK                      
  ARTIKELSTATISTIK    ARTIKELSTATISTIK    ARTIKELSTATISTIK                      
  Hej, detta är ett MEMO från FILEMONÖVERFÖRING XFERID=W330B6                   
                                                                                
  Fil WIN.XBMS.W33092(+1)  är nu katalogiserad                                  
                                                                                
  kolla att den verkar ok.                                                      
                                                                                
)END                                                                            
/*                                                                              
