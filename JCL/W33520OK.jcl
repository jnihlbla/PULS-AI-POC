//W33520OK JOB (540W3350100W33520OK,W100),'RTN W335D2',                         
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
  TITLE WIN.XBMS.20                                                             
  OPTION FORCE                                                                  
  DEST  ELEONOR.OSTROM(A)VOLVO.COM                                              
  MEMO                                                                          
                                                                                
  Hej, detta är ett MEMO från FILEMONÖVERFÖRING XFERID=W335d2                   
                                                                                
  Fil WIN.w335d2.W33520(+1)  är nu katalogiserad                                
                                                                                
  kolla att den verkar ok.                                                      
                                                                                
)END                                                                            
/*                                                                              
