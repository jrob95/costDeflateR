devtools::load_all()

httptest::capture_requests({
  get_oecd_ppp()
  # get_imf("*.NGDP_D.*")
  get_imf_gdpd()
  get_imf_ppp()
})
