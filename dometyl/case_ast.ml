lib/generator/case.ml
==>
[{pstr_desc =
   Pstr_open
    {popen_expr = {pmod_desc = Pmod_ident {txt = Lident "Base"}};
     popen_override = Override}};
 {pstr_desc =
   Pstr_open
    {popen_expr = {pmod_desc = Pmod_ident {txt = Lident "Scad_ml"}};
     popen_override = Override}};
 {pstr_desc =
   Pstr_open
    {popen_expr = {pmod_desc = Pmod_ident {txt = Lident "Infix"}};
     popen_override = Override}};
 {pstr_desc =
   Pstr_type (Recursive,
    [{ptype_name = {txt = "t"};
      ptype_params =
       [({ptyp_desc = Ptyp_var "k"; ptyp_loc_stack = []},
         (NoVariance, NoInjectivity))];
      ptype_cstrs = [];
      ptype_kind =
       Ptype_record
        [{pld_name = {txt = "scad"}; pld_mutable = Immutable;
          pld_type =
           {ptyp_desc = Ptyp_constr ({txt = Ldot (Lident "Model", "t")}, []);
            ptyp_loc_stack = []}};
         {pld_name = {txt = "plate"}; pld_mutable = Immutable;
          pld_type =
           {ptyp_desc =
             Ptyp_constr ({txt = Ldot (Lident "Plate", "t")},
              [{ptyp_desc = Ptyp_var "k"; ptyp_loc_stack = []}]);
            ptyp_loc_stack = []}};
         {pld_name = {txt = "walls"}; pld_mutable = Immutable;
          pld_type =
           {ptyp_desc = Ptyp_constr ({txt = Ldot (Lident "Walls", "t")}, []);
            ptyp_loc_stack = []}};
         {pld_name = {txt = "connections"}; pld_mutable = Immutable;
          pld_type =
           {ptyp_desc =
             Ptyp_constr ({txt = Ldot (Lident "Connect", "t")}, []);
            ptyp_loc_stack = []}}];
      ptype_private = Public; ptype_manifest = None}])};
 {pstr_desc =
   Pstr_value (Nonrecursive,
    [{pvb_pat =
       {ppat_desc = Ppat_var {txt = "translate"}; ppat_loc_stack = []};
      pvb_expr =
       {pexp_desc =
         Pexp_fun (Nolabel, None,
          {ppat_desc = Ppat_var {txt = "p"}; ppat_loc_stack = []},
          {pexp_desc =
            Pexp_fun (Nolabel, None,
             {ppat_desc = Ppat_var {txt = "t"}; ppat_loc_stack = []},
             {pexp_desc =
               Pexp_record
                ([({txt = Lident "scad"},
                   {pexp_desc =
                     Pexp_apply
                      ({pexp_desc =
                         Pexp_ident
                          {txt = Ldot (Lident "Model", "translate")};
                        pexp_loc_stack = []},
                      [(Nolabel,
                        {pexp_desc = Pexp_ident {txt = Lident "p"};
                         pexp_loc_stack = []});
                       (Nolabel,
                        {pexp_desc =
                          Pexp_field
                           ({pexp_desc = Pexp_ident {txt = Lident "t"};
                             pexp_loc_stack = []},
                           {txt = Lident "scad"});
                         pexp_loc_stack = []})]);
                    pexp_loc_stack = []});
                  ({txt = Lident "plate"},
                   {pexp_desc =
                     Pexp_apply
                      ({pexp_desc =
                         Pexp_ident
                          {txt = Ldot (Lident "Plate", "translate")};
                        pexp_loc_stack = []},
                      [(Nolabel,
                        {pexp_desc = Pexp_ident {txt = Lident "p"};
                         pexp_loc_stack = []});
                       (Nolabel,
                        {pexp_desc =
                          Pexp_field
                           ({pexp_desc = Pexp_ident {txt = Lident "t"};
                             pexp_loc_stack = []},
                           {txt = Lident "plate"});
                         pexp_loc_stack = []})]);
                    pexp_loc_stack = []});
                  ({txt = Lident "walls"},
                   {pexp_desc =
                     Pexp_apply
                      ({pexp_desc =
                         Pexp_ident
                          {txt = Ldot (Lident "Walls", "translate")};
                        pexp_loc_stack = []},
                      [(Nolabel,
                        {pexp_desc = Pexp_ident {txt = Lident "p"};
                         pexp_loc_stack = []});
                       (Nolabel,
                        {pexp_desc =
                          Pexp_field
                           ({pexp_desc = Pexp_ident {txt = Lident "t"};
                             pexp_loc_stack = []},
                           {txt = Lident "walls"});
                         pexp_loc_stack = []})]);
                    pexp_loc_stack = []});
                  ({txt = Lident "connections"},
                   {pexp_desc =
                     Pexp_apply
                      ({pexp_desc =
                         Pexp_ident
                          {txt = Ldot (Lident "Connect", "translate")};
                        pexp_loc_stack = []},
                      [(Nolabel,
                        {pexp_desc = Pexp_ident {txt = Lident "p"};
                         pexp_loc_stack = []});
                       (Nolabel,
                        {pexp_desc =
                          Pexp_field
                           ({pexp_desc = Pexp_ident {txt = Lident "t"};
                             pexp_loc_stack = []},
                           {txt = Lident "connections"});
                         pexp_loc_stack = []})]);
                    pexp_loc_stack = []})],
                None);
              pexp_loc_stack = []});
           pexp_loc_stack = []});
        pexp_loc_stack = []}}])};
 {pstr_desc =
   Pstr_value (Nonrecursive,
    [{pvb_pat = {ppat_desc = Ppat_var {txt = "mirror"}; ppat_loc_stack = []};
      pvb_expr =
       {pexp_desc =
         Pexp_fun (Nolabel, None,
          {ppat_desc = Ppat_var {txt = "ax"}; ppat_loc_stack = []},
          {pexp_desc =
            Pexp_fun (Nolabel, None,
             {ppat_desc = Ppat_var {txt = "t"}; ppat_loc_stack = []},
             {pexp_desc =
               Pexp_record
                ([({txt = Lident "scad"},
                   {pexp_desc =
                     Pexp_apply
                      ({pexp_desc =
                         Pexp_ident {txt = Ldot (Lident "Model", "mirror")};
                        pexp_loc_stack = []},
                      [(Nolabel,
                        {pexp_desc = Pexp_ident {txt = Lident "ax"};
                         pexp_loc_stack = []});
                       (Nolabel,
                        {pexp_desc =
                          Pexp_field
                           ({pexp_desc = Pexp_ident {txt = Lident "t"};
                             pexp_loc_stack = []},
                           {txt = Lident "scad"});
                         pexp_loc_stack = []})]);
                    pexp_loc_stack = []});
                  ({txt = Lident "plate"},
                   {pexp_desc =
                     Pexp_apply
                      ({pexp_desc =
                         Pexp_ident {txt = Ldot (Lident "Plate", "mirror")};
                        pexp_loc_stack = []},
                      [(Nolabel,
                        {pexp_desc = Pexp_ident {txt = Lident "ax"};
                         pexp_loc_stack = []});
                       (Nolabel,
                        {pexp_desc =
                          Pexp_field
                           ({pexp_desc = Pexp_ident {txt = Lident "t"};
                             pexp_loc_stack = []},
                           {txt = Lident "plate"});
                         pexp_loc_stack = []})]);
                    pexp_loc_stack = []});
                  ({txt = Lident "walls"},
                   {pexp_desc =
                     Pexp_apply
                      ({pexp_desc =
                         Pexp_ident {txt = Ldot (Lident "Walls", "mirror")};
                        pexp_loc_stack = []},
                      [(Nolabel,
                        {pexp_desc = Pexp_ident {txt = Lident "ax"};
                         pexp_loc_stack = []});
                       (Nolabel,
                        {pexp_desc =
                          Pexp_field
                           ({pexp_desc = Pexp_ident {txt = Lident "t"};
                             pexp_loc_stack = []},
                           {txt = Lident "walls"});
                         pexp_loc_stack = []})]);
                    pexp_loc_stack = []});
                  ({txt = Lident "connections"},
                   {pexp_desc =
                     Pexp_apply
                      ({pexp_desc =
                         Pexp_ident {txt = Ldot (Lident "Connect", "mirror")};
                        pexp_loc_stack = []},
                      [(Nolabel,
                        {pexp_desc = Pexp_ident {txt = Lident "ax"};
                         pexp_loc_stack = []});
                       (Nolabel,
                        {pexp_desc =
                          Pexp_field
                           ({pexp_desc = Pexp_ident {txt = Lident "t"};
                             pexp_loc_stack = []},
                           {txt = Lident "connections"});
                         pexp_loc_stack = []})]);
                    pexp_loc_stack = []})],
                None);
              pexp_loc_stack = []});
           pexp_loc_stack = []});
        pexp_loc_stack = []}}])};
 {pstr_desc =
   Pstr_value (Nonrecursive,
    [{pvb_pat = {ppat_desc = Ppat_var {txt = "rotate"}; ppat_loc_stack = []};
      pvb_expr =
       {pexp_desc =
         Pexp_fun (Nolabel, None,
          {ppat_desc = Ppat_var {txt = "r"}; ppat_loc_stack = []},
          {pexp_desc =
            Pexp_fun (Nolabel, None,
             {ppat_desc = Ppat_var {txt = "t"}; ppat_loc_stack = []},
             {pexp_desc =
               Pexp_record
                ([({txt = Lident "scad"},
                   {pexp_desc =
                     Pexp_apply
                      ({pexp_desc =
                         Pexp_ident {txt = Ldot (Lident "Model", "rotate")};
                        pexp_loc_stack = []},
                      [(Nolabel,
                        {pexp_desc = Pexp_ident {txt = Lident "r"};
                         pexp_loc_stack = []});
                       (Nolabel,
                        {pexp_desc =
                          Pexp_field
                           ({pexp_desc = Pexp_ident {txt = Lident "t"};
                             pexp_loc_stack = []},
                           {txt = Lident "scad"});
                         pexp_loc_stack = []})]);
                    pexp_loc_stack = []});
                  ({txt = Lident "plate"},
                   {pexp_desc =
                     Pexp_apply
                      ({pexp_desc =
                         Pexp_ident {txt = Ldot (Lident "Plate", "rotate")};
                        pexp_loc_stack = []},
                      [(Nolabel,
                        {pexp_desc = Pexp_ident {txt = Lident "r"};
                         pexp_loc_stack = []});
                       (Nolabel,
                        {pexp_desc =
                          Pexp_field
                           ({pexp_desc = Pexp_ident {txt = Lident "t"};
                             pexp_loc_stack = []},
                           {txt = Lident "plate"});
                         pexp_loc_stack = []})]);
                    pexp_loc_stack = []});
                  ({txt = Lident "walls"},
                   {pexp_desc =
                     Pexp_apply
                      ({pexp_desc =
                         Pexp_ident {txt = Ldot (Lident "Walls", "rotate")};
                        pexp_loc_stack = []},
                      [(Nolabel,
                        {pexp_desc = Pexp_ident {txt = Lident "r"};
                         pexp_loc_stack = []});
                       (Nolabel,
                        {pexp_desc =
                          Pexp_field
                           ({pexp_desc = Pexp_ident {txt = Lident "t"};
                             pexp_loc_stack = []},
                           {txt = Lident "walls"});
                         pexp_loc_stack = []})]);
                    pexp_loc_stack = []});
                  ({txt = Lident "connections"},
                   {pexp_desc =
                     Pexp_apply
                      ({pexp_desc =
                         Pexp_ident {txt = Ldot (Lident "Connect", "rotate")};
                        pexp_loc_stack = []},
                      [(Nolabel,
                        {pexp_desc = Pexp_ident {txt = Lident "r"};
                         pexp_loc_stack = []});
                       (Nolabel,
                        {pexp_desc =
                          Pexp_field
                           ({pexp_desc = Pexp_ident {txt = Lident "t"};
                             pexp_loc_stack = []},
                           {txt = Lident "connections"});
                         pexp_loc_stack = []})]);
                    pexp_loc_stack = []})],
                None);
              pexp_loc_stack = []});
           pexp_loc_stack = []});
        pexp_loc_stack = []}}])};
 {pstr_desc =
   Pstr_value (Nonrecursive,
    [{pvb_pat =
       {ppat_desc = Ppat_var {txt = "rotate_about_pt"}; ppat_loc_stack = []};
      pvb_expr =
       {pexp_desc =
         Pexp_fun (Nolabel, None,
          {ppat_desc = Ppat_var {txt = "r"}; ppat_loc_stack = []},
          {pexp_desc =
            Pexp_fun (Nolabel, None,
             {ppat_desc = Ppat_var {txt = "p"}; ppat_loc_stack = []},
             {pexp_desc =
               Pexp_fun (Nolabel, None,
                {ppat_desc = Ppat_var {txt = "t"}; ppat_loc_stack = []},
                {pexp_desc =
                  Pexp_record
                   ([({txt = Lident "scad"},
                      {pexp_desc =
                        Pexp_apply
                         ({pexp_desc =
                            Pexp_ident
                             {txt = Ldot (Lident "Model", "rotate_about_pt")};
                           pexp_loc_stack = []},
                         [(Nolabel,
                           {pexp_desc = Pexp_ident {txt = Lident "r"};
                            pexp_loc_stack = []});
                          (Nolabel,
                           {pexp_desc = Pexp_ident {txt = Lident "p"};
                            pexp_loc_stack = []});
                          (Nolabel,
                           {pexp_desc =
                             Pexp_field
                              ({pexp_desc = Pexp_ident {txt = Lident "t"};
                                pexp_loc_stack = []},
                              {txt = Lident "scad"});
                            pexp_loc_stack = []})]);
                       pexp_loc_stack = []});
                     ({txt = Lident "plate"},
                      {pexp_desc =
                        Pexp_apply
                         ({pexp_desc =
                            Pexp_ident
                             {txt = Ldot (Lident "Plate", "rotate_about_pt")};
                           pexp_loc_stack = []},
                         [(Nolabel,
                           {pexp_desc = Pexp_ident {txt = Lident "r"};
                            pexp_loc_stack = []});
                          (Nolabel,
                           {pexp_desc = Pexp_ident {txt = Lident "p"};
                            pexp_loc_stack = []});
                          (Nolabel,
                           {pexp_desc =
                             Pexp_field
                              ({pexp_desc = Pexp_ident {txt = Lident "t"};
                                pexp_loc_stack = []},
                              {txt = Lident "plate"});
                            pexp_loc_stack = []})]);
                       pexp_loc_stack = []});
                     ({txt = Lident "walls"},
                      {pexp_desc =
                        Pexp_apply
                         ({pexp_desc =
                            Pexp_ident
                             {txt = Ldot (Lident "Walls", "rotate_about_pt")};
                           pexp_loc_stack = []},
                         [(Nolabel,
                           {pexp_desc = Pexp_ident {txt = Lident "r"};
                            pexp_loc_stack = []});
                          (Nolabel,
                           {pexp_desc = Pexp_ident {txt = Lident "p"};
                            pexp_loc_stack = []});
                          (Nolabel,
                           {pexp_desc =
                             Pexp_field
                              ({pexp_desc = Pexp_ident {txt = Lident "t"};
                                pexp_loc_stack = []},
                              {txt = Lident "walls"});
                            pexp_loc_stack = []})]);
                       pexp_loc_stack = []});
                     ({txt = Lident "connections"},
                      {pexp_desc =
                        Pexp_apply
                         ({pexp_desc =
                            Pexp_ident
                             {txt =
                               Ldot (Lident "Connect", "rotate_about_pt")};
                           pexp_loc_stack = []},
                         [(Nolabel,
                           {pexp_desc = Pexp_ident {txt = Lident "r"};
                            pexp_loc_stack = []});
                          (Nolabel,
                           {pexp_desc = Pexp_ident {txt = Lident "p"};
                            pexp_loc_stack = []});
                          (Nolabel,
                           {pexp_desc =
                             Pexp_field
                              ({pexp_desc = Pexp_ident {txt = Lident "t"};
                                pexp_loc_stack = []},
                              {txt = Lident "connections"});
                            pexp_loc_stack = []})]);
                       pexp_loc_stack = []})],
                   None);
                 pexp_loc_stack = []});
              pexp_loc_stack = []});
           pexp_loc_stack = []});
        pexp_loc_stack = []}}])};
 {pstr_desc =
   Pstr_value (Nonrecursive,
    [{pvb_pat = {ppat_desc = Ppat_var {txt = "make"}; ppat_loc_stack = []};
      pvb_expr =
       {pexp_desc =
         Pexp_fun (Optional "right_hand",
          Some
           {pexp_desc = Pexp_construct ({txt = Lident "true"}, None);
            pexp_loc_stack = []},
          {ppat_desc = Ppat_var {txt = "right_hand"}; ppat_loc_stack = []},
          {pexp_desc =
            Pexp_fun (Labelled "plate_builder", None,
             {ppat_desc = Ppat_var {txt = "plate_builder"};
              ppat_loc_stack = []},
             {pexp_desc =
               Pexp_fun (Labelled "plate_welder", None,
                {ppat_desc = Ppat_var {txt = "plate_welder"};
                 ppat_loc_stack = []},
                {pexp_desc =
                  Pexp_fun (Labelled "wall_builder", None,
                   {ppat_desc = Ppat_var {txt = "wall_builder"};
                    ppat_loc_stack = []},
                   {pexp_desc =
                     Pexp_fun (Labelled "base_connector", None,
                      {ppat_desc = Ppat_var {txt = "base_connector"};
                       ppat_loc_stack = []},
                      {pexp_desc =
                        Pexp_fun (Labelled "ports_cutter", None,
                         {ppat_desc = Ppat_var {txt = "ports_cutter"};
                          ppat_loc_stack = []},
                         {pexp_desc =
                           Pexp_fun (Nolabel, None,
                            {ppat_desc = Ppat_var {txt = "keyhole"};
                             ppat_loc_stack = []},
                            {pexp_desc =
                              Pexp_let (Nonrecursive,
                               [{pvb_pat =
                                  {ppat_desc = Ppat_var {txt = "plate"};
                                   ppat_loc_stack = []};
                                 pvb_expr =
                                  {pexp_desc =
                                    Pexp_apply
                                     ({pexp_desc =
                                        Pexp_ident
                                         {txt = Lident "plate_builder"};
                                       pexp_loc_stack = []},
                                     [(Nolabel,
                                       {pexp_desc =
                                         Pexp_ifthenelse
                                          ({pexp_desc =
                                             Pexp_ident
                                              {txt = Lident "right_hand"};
                                            pexp_loc_stack = []},
                                          {pexp_desc =
                                            Pexp_ident
                                             {txt = Lident "keyhole"};
                                           pexp_loc_stack = []},
                                          Some
                                           {pexp_desc =
                                             Pexp_apply
                                              ({pexp_desc =
                                                 Pexp_ident
                                                  {txt =
                                                    Ldot (Lident "KeyHole",
                                                     "mirror_internals")};
                                                pexp_loc_stack = []},
                                              [(Nolabel,
                                                {pexp_desc =
                                                  Pexp_ident
                                                   {txt = Lident "keyhole"};
                                                 pexp_loc_stack = []})]);
                                            pexp_loc_stack = []});
                                        pexp_loc_stack = [...]})]);
                                   pexp_loc_stack = []}}],
                               {pexp_desc =
                                 Pexp_let (Nonrecursive,
                                  [{pvb_pat =
                                     {ppat_desc = Ppat_var {txt = "walls"};
                                      ppat_loc_stack = []};
                                    pvb_expr =
                                     {pexp_desc =
                                       Pexp_apply
                                        ({pexp_desc =
                                           Pexp_ident
                                            {txt = Lident "wall_builder"};
                                          pexp_loc_stack = []},
                                        [(Nolabel,
                                          {pexp_desc =
                                            Pexp_ident {txt = Lident "plate"};
                                           pexp_loc_stack = []})]);
                                      pexp_loc_stack = []}}],
                                  {pexp_desc =
                                    Pexp_let (Nonrecursive,
                                     [{pvb_pat =
                                        {ppat_desc =
                                          Ppat_var {txt = "connections"};
                                         ppat_loc_stack = []};
                                       pvb_expr =
                                        {pexp_desc =
                                          Pexp_apply
                                           ({pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "base_connector"};
                                             pexp_loc_stack = []},
                                           [(Nolabel,
                                             {pexp_desc =
                                               Pexp_ident
                                                {txt = Lident "walls"};
                                              pexp_loc_stack = []})]);
                                         pexp_loc_stack = []}}],
                                     {pexp_desc =
                                       Pexp_let (Nonrecursive,
                                        [{pvb_pat =
                                           {ppat_desc = Ppat_var {txt = "t"};
                                            ppat_loc_stack = []};
                                          pvb_expr =
                                           {pexp_desc =
                                             Pexp_record
                                              ([({txt = Lident "scad"},
                                                 {pexp_desc =
                                                   Pexp_apply
                                                    ({pexp_desc =
                                                       Pexp_ident
                                                        {txt = Lident "|>"};
                                                      pexp_loc_stack = []},
                                                    [(Nolabel,
                                                      {pexp_desc =
                                                        Pexp_apply
                                                         ({pexp_desc =
                                                            Pexp_ident
                                                             {txt =
                                                               Ldot
                                                                (Lident
                                                                  "Model",
                                                                "difference")};
                                                           pexp_loc_stack =
                                                            []},
                                                         [(Nolabel,
                                                           {pexp_desc =
                                                             Pexp_apply
                                                              ({pexp_desc =
                                                                 Pexp_ident
                                                                  {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Model",
                                                                    "union")};
                                                                pexp_loc_stack
                                                                 = []},
                                                              [(Nolabel,
                                                                {pexp_desc =
                                                                  Pexp_construct
                                                                   ({txt =
                                                                    Lident
                                                                    "::"},
                                                                   Some
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Plate",
                                                                    "to_scad")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "plate"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_construct
                                                                    ({txt =
                                                                    Lident
                                                                    "::"},
                                                                    Some
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Walls",
                                                                    "to_scad")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "walls"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_construct
                                                                    ({txt =
                                                                    Lident
                                                                    "::"},
                                                                    Some
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Connect",
                                                                    "to_scad")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "connections"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_construct
                                                                    ({txt =
                                                                    Lident
                                                                    "::"},
                                                                    Some
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_tuple
                                                                    [{pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "plate_welder"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "plate"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_construct
                                                                    ({txt =
                                                                    Lident
                                                                    "[]"},
                                                                    None);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []});
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []});
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []});
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []});
                                                                 pexp_loc_stack
                                                                  = []})]);
                                                            pexp_loc_stack =
                                                             [...]});
                                                          (Nolabel,
                                                           {pexp_desc =
                                                             Pexp_construct
                                                              ({txt =
                                                                 Lident "::"},
                                                              Some
                                                               {pexp_desc =
                                                                 Pexp_tuple
                                                                  [{pexp_desc
                                                                    =
                                                                    Pexp_apply
                                                                    ({pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Ldot
                                                                    (Lident
                                                                    "Plate",
                                                                    "collect_cutouts")};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []},
                                                                    [(Nolabel,
                                                                    {pexp_desc
                                                                    =
                                                                    Pexp_ident
                                                                    {txt =
                                                                    Lident
                                                                    "plate"};
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []})]);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []};
                                                                   {pexp_desc
                                                                    =
                                                                    Pexp_construct
                                                                    ({txt =
                                                                    Lident
                                                                    "[]"},
                                                                    None);
                                                                    pexp_loc_stack
                                                                    = 
                                                                    []}];
                                                                pexp_loc_stack
                                                                 = []});
                                                            pexp_loc_stack =
                                                             []})]);
                                                       pexp_loc_stack = []});
                                                     (Nolabel,
                                                      {pexp_desc =
                                                        Pexp_apply
                                                         ({pexp_desc =
                                                            Pexp_ident
                                                             {txt =
                                                               Ldot
                                                                (Lident
                                                                  "Ports",
                                                                "apply")};
                                                           pexp_loc_stack =
                                                            []},
                                                         [(Nolabel,
                                                           {pexp_desc =
                                                             Pexp_apply
                                                              ({pexp_desc =
                                                                 Pexp_ident
                                                                  {txt =
                                                                    Lident
                                                                    "ports_cutter"};
                                                                pexp_loc_stack
                                                                 = []},
                                                              [(Labelled
                                                                 "walls",
                                                                {pexp_desc =
                                                                  Pexp_ident
                                                                   {txt =
                                                                    Lident
                                                                    "walls"};
                                                                 pexp_loc_stack
                                                                  = []});
                                                               (Labelled
                                                                 "connections",
                                                                {pexp_desc =
                                                                  Pexp_ident
                                                                   {txt =
                                                                    Lident
                                                                    "connections"};
                                                                 pexp_loc_stack
                                                                  = []})]);
                                                            pexp_loc_stack =
                                                             [...]})]);
                                                       pexp_loc_stack = []})]);
                                                  pexp_loc_stack = []});
                                                ({txt = Lident "plate"},
                                                 {pexp_desc =
                                                   Pexp_ident
                                                    {txt = Lident "plate"};
                                                  pexp_loc_stack = []});
                                                ({txt = Lident "walls"},
                                                 {pexp_desc =
                                                   Pexp_ident
                                                    {txt = Lident "walls"};
                                                  pexp_loc_stack = []});
                                                ({txt = Lident "connections"},
                                                 {pexp_desc =
                                                   Pexp_ident
                                                    {txt =
                                                      Lident "connections"};
                                                  pexp_loc_stack = []})],
                                              None);
                                            pexp_loc_stack = []}}],
                                        {pexp_desc =
                                          Pexp_ifthenelse
                                           ({pexp_desc =
                                              Pexp_ident
                                               {txt = Lident "right_hand"};
                                             pexp_loc_stack = []},
                                           {pexp_desc =
                                             Pexp_ident {txt = Lident "t"};
                                            pexp_loc_stack = []},
                                           Some
                                            {pexp_desc =
                                              Pexp_apply
                                               ({pexp_desc =
                                                  Pexp_ident
                                                   {txt = Lident "mirror"};
                                                 pexp_loc_stack = []},
                                               [(Nolabel,
                                                 {pexp_desc =
                                                   Pexp_tuple
                                                    [{pexp_desc =
                                                       Pexp_constant
                                                        (Pconst_float ("1.",
                                                          None));
                                                      pexp_loc_stack = []};
                                                     {pexp_desc =
                                                       Pexp_constant
                                                        (Pconst_float ("0.",
                                                          None));
                                                      pexp_loc_stack = []};
                                                     {pexp_desc =
                                                       Pexp_constant
                                                        (Pconst_float ("0.",
                                                          None));
                                                      pexp_loc_stack = []}];
                                                  pexp_loc_stack = [...]});
                                                (Nolabel,
                                                 {pexp_desc =
                                                   Pexp_ident
                                                    {txt = Lident "t"};
                                                  pexp_loc_stack = []})]);
                                             pexp_loc_stack = []});
                                         pexp_loc_stack = []});
                                      pexp_loc_stack = []});
                                   pexp_loc_stack = []});
                                pexp_loc_stack = []});
                             pexp_loc_stack = []});
                          pexp_loc_stack = []});
                       pexp_loc_stack = []});
                    pexp_loc_stack = []});
                 pexp_loc_stack = []});
              pexp_loc_stack = []});
           pexp_loc_stack = []});
        pexp_loc_stack = []}}])};
 {pstr_desc =
   Pstr_value (Nonrecursive,
    [{pvb_pat = {ppat_desc = Ppat_var {txt = "to_scad"}; ppat_loc_stack = []};
      pvb_expr =
       {pexp_desc =
         Pexp_fun (Optional "show_caps",
          Some
           {pexp_desc = Pexp_construct ({txt = Lident "false"}, None);
            pexp_loc_stack = []},
          {ppat_desc = Ppat_var {txt = "show_caps"}; ppat_loc_stack = []},
          {pexp_desc =
            Pexp_fun (Optional "show_cutouts",
             Some
              {pexp_desc = Pexp_construct ({txt = Lident "false"}, None);
               pexp_loc_stack = []},
             {ppat_desc = Ppat_var {txt = "show_cutouts"};
              ppat_loc_stack = []},
             {pexp_desc =
               Pexp_fun (Nolabel, None,
                {ppat_desc = Ppat_var {txt = "t"}; ppat_loc_stack = []},
                {pexp_desc =
                  Pexp_let (Nonrecursive,
                   [{pvb_pat =
                      {ppat_desc = Ppat_var {txt = "caps"};
                       ppat_loc_stack = []};
                     pvb_expr =
                      {pexp_desc =
                        Pexp_ifthenelse
                         ({pexp_desc = Pexp_ident {txt = Lident "show_caps"};
                           pexp_loc_stack = []},
                         {pexp_desc =
                           Pexp_construct ({txt = Lident "Some"},
                            Some
                             {pexp_desc =
                               Pexp_apply
                                ({pexp_desc =
                                   Pexp_ident
                                    {txt =
                                      Ldot (Lident "Plate", "collect_caps")};
                                  pexp_loc_stack = []},
                                [(Nolabel,
                                  {pexp_desc =
                                    Pexp_field
                                     ({pexp_desc =
                                        Pexp_ident {txt = Lident "t"};
                                       pexp_loc_stack = []},
                                     {txt = Lident "plate"});
                                   pexp_loc_stack = []})]);
                              pexp_loc_stack = [...]});
                          pexp_loc_stack = []},
                         Some
                          {pexp_desc =
                            Pexp_construct ({txt = Lident "None"}, None);
                           pexp_loc_stack = []});
                       pexp_loc_stack = []}};
                    {pvb_pat =
                      {ppat_desc = Ppat_var {txt = "cutouts"};
                       ppat_loc_stack = []};
                     pvb_expr =
                      {pexp_desc =
                        Pexp_ifthenelse
                         ({pexp_desc =
                            Pexp_ident {txt = Lident "show_cutouts"};
                           pexp_loc_stack = []},
                         {pexp_desc =
                           Pexp_construct ({txt = Lident "Some"},
                            Some
                             {pexp_desc =
                               Pexp_apply
                                ({pexp_desc =
                                   Pexp_ident
                                    {txt = Ldot (Lident "Model", "color")};
                                  pexp_loc_stack = []},
                                [(Nolabel,
                                  {pexp_desc =
                                    Pexp_construct
                                     ({txt = Ldot (Lident "Color", "Black")},
                                     None);
                                   pexp_loc_stack = []});
                                 (Nolabel,
                                  {pexp_desc =
                                    Pexp_apply
                                     ({pexp_desc =
                                        Pexp_ident
                                         {txt =
                                           Ldot (Lident "Plate",
                                            "collect_cutouts")};
                                       pexp_loc_stack = []},
                                     [(Nolabel,
                                       {pexp_desc =
                                         Pexp_field
                                          ({pexp_desc =
                                             Pexp_ident {txt = Lident "t"};
                                            pexp_loc_stack = []},
                                          {txt = Lident "plate"});
                                        pexp_loc_stack = []})]);
                                   pexp_loc_stack = [...]})]);
                              pexp_loc_stack = [...]});
                          pexp_loc_stack = []},
                         Some
                          {pexp_desc =
                            Pexp_construct ({txt = Lident "None"}, None);
                           pexp_loc_stack = []});
                       pexp_loc_stack = []}}],
                   {pexp_desc =
                     Pexp_apply
                      ({pexp_desc = Pexp_ident {txt = Lident "|>"};
                        pexp_loc_stack = []},
                      [(Nolabel,
                        {pexp_desc =
                          Pexp_apply
                           ({pexp_desc = Pexp_ident {txt = Lident "|>"};
                             pexp_loc_stack = []},
                           [(Nolabel,
                             {pexp_desc =
                               Pexp_apply
                                ({pexp_desc = Pexp_ident {txt = Lident "|>"};
                                  pexp_loc_stack = []},
                                [(Nolabel,
                                  {pexp_desc =
                                    Pexp_construct ({txt = Lident "::"},
                                     Some
                                      {pexp_desc =
                                        Pexp_tuple
                                         [{pexp_desc =
                                            Pexp_field
                                             ({pexp_desc =
                                                Pexp_ident {txt = Lident "t"};
                                               pexp_loc_stack = []},
                                             {txt = Lident "scad"});
                                           pexp_loc_stack = []};
                                          {pexp_desc =
                                            Pexp_construct
                                             ({txt = Lident "[]"}, None);
                                           pexp_loc_stack = []}];
                                       pexp_loc_stack = []});
                                   pexp_loc_stack = []});
                                 (Nolabel,
                                  {pexp_desc =
                                    Pexp_apply
                                     ({pexp_desc =
                                        Pexp_ident
                                         {txt =
                                           Ldot (Lident "Util",
                                            "prepend_opt")};
                                       pexp_loc_stack = []},
                                     [(Nolabel,
                                       {pexp_desc =
                                         Pexp_ident {txt = Lident "caps"};
                                        pexp_loc_stack = []})]);
                                   pexp_loc_stack = []})]);
                              pexp_loc_stack = []});
                            (Nolabel,
                             {pexp_desc =
                               Pexp_apply
                                ({pexp_desc =
                                   Pexp_ident
                                    {txt =
                                      Ldot (Lident "Util", "prepend_opt")};
                                  pexp_loc_stack = []},
                                [(Nolabel,
                                  {pexp_desc =
                                    Pexp_ident {txt = Lident "cutouts"};
                                   pexp_loc_stack = []})]);
                              pexp_loc_stack = []})]);
                         pexp_loc_stack = []});
                       (Nolabel,
                        {pexp_desc =
                          Pexp_ident {txt = Ldot (Lident "Model", "union")};
                         pexp_loc_stack = []})]);
                    pexp_loc_stack = []});
                 pexp_loc_stack = []});
              pexp_loc_stack = []});
           pexp_loc_stack = []});
        pexp_loc_stack = []}}])}]
=========
