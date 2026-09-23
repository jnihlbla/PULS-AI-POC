//W37199OK JOB (540W3710100W37199OK,W100),'RTN W371B5',                         
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
  TITLE WIN.XBMS                                                                
  OPTION FORCE                                                                  
  DEST  ELEONOR.OSTROM(A)VOLVO.COM                                              
  MEMO                                                                          
                                                                                
  Hej, detta är ett MEMO från FILEMONÖVERFÖRING XFERID=W371B5                   
                                                                                
  Fil WIN.W37199(+1)  är nu katalogiserad                                       
                                                                                
  kolla att den verkar ok.                                                      
                                                                                
)END                                                                            
/*                                                                              
